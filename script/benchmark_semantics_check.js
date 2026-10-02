#!/usr/bin/env node
'use strict';
const fs=require('fs');
const cases=['add-1','add-5','add-all','datoms-name','q1','q2','q3','q4','q5-shortcircuit','qpred1','qpred2','q2pred','pull-one','get-page-data'].sort();
function difference(expected,actual,path='snapshot') {
  if(expected===actual)return null;
  if(expected===null||actual===null||typeof expected!=='object'||typeof actual!=='object')return path;
  if(Array.isArray(expected)!==Array.isArray(actual))return path;
  const left=Object.keys(expected).sort(),right=Object.keys(actual).sort();
  if(JSON.stringify(left)!==JSON.stringify(right))return path+'.keys';
  for(const key of left){const mismatch=difference(expected[key],actual[key],path+'.'+key);if(mismatch)return mismatch;}
  return null;
}
function check(snapshots) {
  if(snapshots.length!==3)throw Error('semantic preflight requires native, js_of_ocaml and CLJS snapshots');
  for(const snapshot of snapshots){
    if(!snapshot||!Number.isInteger(snapshot.size)||snapshot.size<0||!snapshot.fixtures||!snapshot.schemas||!snapshot.queries||!snapshot.cases)throw Error('invalid semantic snapshot');
    if(JSON.stringify(Object.keys(snapshot.cases).sort())!==JSON.stringify(cases))throw Error('semantic snapshot must cover all comparable benchmarks');
  }
  for(let i=1;i<snapshots.length;i++){
    const mismatch=difference(snapshots[0],snapshots[i]);
    if(mismatch)throw Error('semantic mismatch for '+['ocaml-native','js_of_ocaml','upstream-cljs-js'][i]+' at '+mismatch);
  }
}
if(require.main===module){try{check(process.argv.slice(2).map(file=>JSON.parse(fs.readFileSync(file,'utf8'))));console.error('semantic preflight: all 14 comparable workloads agree');}catch(e){console.error('semantic preflight: '+e.message);process.exit(1);}}
module.exports={check};
