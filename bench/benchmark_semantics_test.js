#!/usr/bin/env node
'use strict';
const assert=require('node:assert/strict'),fs=require('fs'),os=require('os'),path=require('path'),cp=require('child_process');
const [nativeArg,jsooArg]=process.argv.slice(2),native=path.resolve(nativeArg),jsoo=path.resolve(jsooArg),root=path.resolve(__dirname,'..'),bundle=process.env.UPSTREAM_DATASCRIPT_JS;
if(!bundle)throw Error('UPSTREAM_DATASCRIPT_JS is required for benchmark semantic regression tests');
const checker=require('../script/benchmark_gate_vs_cljs_check.js');
let failures=0;
function test(name,fn){try{fn();console.log('PASS '+name);}catch(e){failures++;console.error('FAIL '+name+': '+e.message);}}
function exported(command,size){const p=cp.spawnSync(command[0],[...command.slice(1),'--size',String(size),'--semantic-only'],{env:process.env,encoding:'utf8',maxBuffer:32*1024*1024});assert.equal(p.status,0,'semantic export must succeed: '+p.stderr.slice(-200));return JSON.parse(p.stdout);}
function canonical(value){if(Array.isArray(value))return value.map(canonical);if(value&&typeof value==='object')return Object.fromEntries(Object.keys(value).sort().map(k=>[k,canonical(value[k])]));return value;}
for(const size of [0,1,20,1000])test('all workload inputs and complete outputs agree at size '+size,()=>{
 const results=[exported([native],size),exported([process.execPath,jsoo],size),exported([process.execPath,path.join(__dirname,'bench_upstream.js')],size)];
 assert.deepEqual(canonical(results[1]),canonical(results[0]));assert.deepEqual(canonical(results[2]),canonical(results[0]));
 assert.equal(Object.keys(results[0].cases).length,14);
 assert.equal(Object.keys(results[0].queries).length,8);
 if(size===1000){assert.equal(results[0].fixtures.people.length,9586);assert.equal(results[0].queries.q3.rows.length,47);}
 if(size===0)assert.equal(results[0].queries.q3.rows.length,0);
});
test('empty and non-finite reference timings cannot pass a strict gate',()=>{
 for(const value of ['', 'q3 NaN\n'])assert.notEqual(checker.check('runtime ocaml-native\nruntime js_of_ocaml\nruntime upstream-cljs-js\n'+value).length,0);
});
const temp=fs.mkdtempSync(path.join(os.tmpdir(),'benchmark-semantic-test-'));
try{
 for(const fault of ['wrong-row','wrong-type'])test('preflight rejects '+fault+' before timing',()=>{
  const wrapper=path.join(temp,fault+'.js');
  const patch=fault==='wrong-row'?`const q=d.q;d.q=function(query,...inputs){const rows=q(query,...inputs);if(query==='[:find ?e :where [?e :name "Ivan"]]'&&rows.length)return rows.map((r,i)=>i===0?[999999]:r);return rows;};`:`const datoms=d.datoms;d.datoms=function(...args){return datoms(...args).map(x=>String(x.a)===':sex'?{e:x.e,a:x.a,v:String(x.v),tx:x.tx,added:x.added}:x);};`;
  fs.writeFileSync(wrapper,'const d=require('+JSON.stringify(path.resolve(bundle))+');'+patch+'module.exports=d;\n');
  const p=cp.spawnSync('bash',[path.join(root,'script/benchmark_vs_cljs.sh')],{cwd:root,encoding:'utf8',env:{...process.env,BENCH_SKIP_BUILD:'1',BENCH_SIZE:'20',BENCH_WARMUP_MS:'1',BENCH_SAMPLE_MS:'1',BENCH_SAMPLES:'1',BENCH_OCAML_NATIVE:native,BENCH_OCAML_JS:jsoo,UPSTREAM_DATASCRIPT_JS:wrapper}});
  assert.notEqual(p.status,0,'different complete results/types must stop comparison');
  assert.match(p.stderr,/semantic/i,'failure must identify semantic mismatch');
  assert.doesNotMatch(p.stdout,/runtime\s+(ocaml-native|js_of_ocaml|upstream-cljs-js)/,'no timed benchmark may start after a failed preflight');
 });
}finally{fs.rmSync(temp,{recursive:true,force:true});}
if(failures)process.exit(1);
