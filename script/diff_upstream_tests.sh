#!/usr/bin/env bash
set -euo pipefail

repo_root="$(cd "$(dirname "$0")/.." && pwd)"
upstream_dir="${UPSTREAM_TEST_DIR:-$repo_root/_deps/datascript/test/datascript/test}"
alias_file="${UPSTREAM_TEST_ALIASES:-$repo_root/test/upstream_test_aliases.tsv}"
strict="${UPSTREAM_TEST_DIFF_STRICT:-0}"

if [ "${1:-}" = "--strict" ]; then
  strict=1
fi

if [ ! -d "$upstream_dir" ]; then
  echo "Upstream test dir not found: $upstream_dir" >&2
  exit 2
fi

tmp_dir="$(mktemp -d)"
trap 'rm -rf "$tmp_dir"' EXIT

upstream_raw="$tmp_dir/upstream.tsv"
upstream_norm="$tmp_dir/upstream-normalized.tsv"
local_tests="$tmp_dir/local.txt"
aliases="$tmp_dir/aliases.tsv"
covered="$tmp_dir/covered.tsv"
missing="$tmp_dir/missing.tsv"
stale="$tmp_dir/stale.tsv"

find "$upstream_dir" -maxdepth 1 \( -name '*.cljc' -o -name '*.cljs' -o -name '*.clj' \) -type f -print0 \
  | sort -z \
  | xargs -0 perl -0777 -ne '
      my $file = $ARGV;
      $file =~ s{.*/}{};
      my @lines = split /\n/, $_;
      for (my $i = 0; $i < @lines; $i++) {
        my $line = $lines[$i];
        next if $line =~ /#_\s*\(deftest/;
        next unless $line =~ /^\s*\(deftest\b(.*)$/;
        my $rest = $1;
        while (1) {
          $rest =~ s/^\s+//;
          $rest =~ s/^\^\{[^}]*\}\s*//;
          $rest =~ s/^\^[^\s]+\s*//;
          if ($rest =~ /^([^\s\)\[]+)/) {
            print "$file\t$1\n";
            last;
          }
          last if ++$i >= @lines;
          $rest = $lines[$i];
        }
      }
    ' > "$upstream_raw"

perl -ne '
  sub norm {
    my ($s) = @_;
    $s =~ s/!/_bang/g;
    $s =~ s/\?/_p/g;
    $s =~ s/[^A-Za-z0-9]+/_/g;
    $s =~ s/^_+|_+$//g;
    return lc $s;
  }
  chomp;
  my ($file, $name) = split /\t/, $_, 2;
  my $base = $file;
  $base =~ s/\.(cljc|cljs|clj)$//;
  print "$file\t$name\ttest_" . norm($base) . "__" . norm($name) . "\t" . norm($name) . "\n";
' "$upstream_raw" > "$upstream_norm"

find "$repo_root/test" -maxdepth 1 -name 'test_*.ml' -type f -print0 \
  | xargs -0 perl -ne 'print "$1\n" if /^let\s+(test_[A-Za-z0-9_]+)\b/' \
  | sort -u > "$local_tests"

if [ -f "$alias_file" ]; then
  perl -ne '
    next if /^\s*(#|$)/;
    chomp;
    my ($file, $upstream, $local) = split /\t/;
    print "$file\t$upstream\t$local\n";
  ' "$alias_file" > "$aliases"
else
  : > "$aliases"
fi

: > "$covered"
: > "$missing"
: > "$stale"

while IFS=$'\t' read -r file upstream suggested loose; do
  alias_target="$(awk -F '\t' -v f="$file" -v u="$upstream" '$1 == f && $2 == u { print $3; found = 1; exit } END { if (!found) exit 1 }' "$aliases" || true)"
  if [ "$alias_target" = "-" ]; then
    printf '%s\t%s\tignored\n' "$file" "$upstream" >> "$covered"
  elif [ -n "$alias_target" ]; then
    if grep -qx "$alias_target" "$local_tests"; then
      printf '%s\t%s\t%s\n' "$file" "$upstream" "$alias_target" >> "$covered"
    else
      printf '%s\t%s\t%s\n' "$file" "$upstream" "$alias_target" >> "$stale"
      printf '%s\t%s\t%s\n' "$file" "$upstream" "$suggested" >> "$missing"
    fi
  elif grep -qx "$suggested" "$local_tests"; then
    printf '%s\t%s\t%s\n' "$file" "$upstream" "$suggested" >> "$covered"
  elif grep -qx "$loose" "$local_tests"; then
    printf '%s\t%s\t%s\n' "$file" "$upstream" "$loose" >> "$covered"
  else
    printf '%s\t%s\t%s\n' "$file" "$upstream" "$suggested" >> "$missing"
  fi
done < "$upstream_norm"

printf 'Upstream tests: %s\n' "$(wc -l < "$upstream_norm" | tr -d ' ')"
printf 'Covered by exact name or alias: %s\n' "$(wc -l < "$covered" | tr -d ' ')"
printf 'Missing name coverage: %s\n' "$(wc -l < "$missing" | tr -d ' ')"
printf 'Stale aliases: %s\n' "$(wc -l < "$stale" | tr -d ' ')"

if [ -s "$stale" ]; then
  printf '\nStale aliases:\n'
  cat "$stale"
fi

if [ -s "$missing" ]; then
  printf '\nMissing upstream test names (file, upstream, suggested OCaml name):\n'
  cat "$missing"
  if [ "$strict" = "1" ]; then
    exit 1
  fi
fi
