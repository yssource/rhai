#![feature(test)]

///! Test evaluating expressions
extern crate test;

use rhai::{Engine, OptimizationLevel};
use test::Bencher;

#[bench]
fn bench_eval_expression_single(bench: &mut Bencher) {
    let script = "1";

    let mut engine = Engine::new();
    engine.set_optimization_level(OptimizationLevel::None);

    let ast = engine.compile_expression(script).unwrap();

    bench.iter(|| engine.consume_ast(&ast).unwrap());
}

#[bench]
fn bench_eval_expression_number_literal(bench: &mut Bencher) {
    let script = "2 > 1";

    let mut engine = Engine::new();
    engine.set_optimization_level(OptimizationLevel::None);

    let ast = engine.compile_expression(script).unwrap();

    bench.iter(|| engine.consume_ast(&ast).unwrap());
}

#[bench]
fn bench_eval_expression_number_operators(bench: &mut Bencher) {
    let script = "2 + 2 == 4";

    let mut engine = Engine::new();
    engine.set_optimization_level(OptimizationLevel::None);

    let ast = engine.compile_expression(script).unwrap();

    bench.iter(|| engine.consume_ast(&ast).unwrap());
}
