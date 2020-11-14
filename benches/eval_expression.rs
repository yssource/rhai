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

#[bench]
fn bench_eval_expression_optimized_simple(bench: &mut Bencher) {
    let script = r#"
            2 > 1 &&
            "something" != "nothing" ||
            "2014-01-20" < "Wed Jul  8 23:07:35 MDT 2015" &&
            [array, has, spaces].len <= #{prop:name}.len &&
            modifierTest + 1000 / 2 > (80 * 100 % 2)
        "#;

    let mut engine = Engine::new();
    engine.set_optimization_level(OptimizationLevel::Simple);
    let ast = engine.compile_expression(script).unwrap();

    bench.iter(|| engine.consume_ast(&ast).unwrap());
}

#[bench]
fn bench_eval_expression_optimized_full(bench: &mut Bencher) {
    let script = r#"
            2 > 1 &&
            "something" != "nothing" ||
            "2014-01-20" < "Wed Jul  8 23:07:35 MDT 2015" &&
            [array, has, spaces].len <= #{prop:name}.len &&
            modifierTest + 1000 / 2 > (80 * 100 % 2)
        "#;

    let mut engine = Engine::new();
    engine.set_optimization_level(OptimizationLevel::Full);
    let ast = engine.compile_expression(script).unwrap();

    bench.iter(|| engine.consume_ast(&ast).unwrap());
}

#[bench]
fn bench_eval_call_expression(bench: &mut Bencher) {
    let script = r#"
            2 > 1 &&
            "something" != "nothing" ||
            "2014-01-20" < "Wed Jul  8 23:07:35 MDT 2015" &&
            [array, has, spaces].len <= #{prop:name}.len &&
            modifierTest + 1000 / 2 > (80 * 100 % 2)
        "#;

    let engine = Engine::new();

    bench.iter(|| engine.eval_expression::<bool>(script).unwrap());
}

#[bench]
fn bench_eval_call(bench: &mut Bencher) {
    let script = r#"
            2 > 1 &&
            "something" != "nothing" ||
            "2014-01-20" < "Wed Jul  8 23:07:35 MDT 2015" &&
            [array, has, spaces].len <= #{prop:name}.len &&
            modifierTest + 1000 / 2 > (80 * 100 % 2)
        "#;

    let engine = Engine::new();

    bench.iter(|| engine.eval::<bool>(script).unwrap());
}

#[bench]
fn bench_eval_loop_number(bench: &mut Bencher) {
    let script = r#"
        let s = 0;
        for x in range(0, 10000) {
            s += 1;
        }
    "#;

    let mut engine = Engine::new();
    engine.set_optimization_level(OptimizationLevel::None);

    let ast = engine.compile(script).unwrap();

    bench.iter(|| engine.consume_ast(&ast).unwrap());
}

#[bench]
fn bench_eval_loop_strings_build(bench: &mut Bencher) {
    let script = r#"
        let s = 0;
        for x in range(0, 10000) {
            s += "x";
        }
    "#;

    let mut engine = Engine::new();
    engine.set_optimization_level(OptimizationLevel::None);

    let ast = engine.compile(script).unwrap();

    bench.iter(|| engine.consume_ast(&ast).unwrap());
}

#[bench]
fn bench_eval_loop_strings_no_build(bench: &mut Bencher) {
    let script = r#"
        let s = "hello";
        for x in range(0, 10000) {
            s += "";
        }
    "#;

    let mut engine = Engine::new();
    engine.set_optimization_level(OptimizationLevel::None);

    let ast = engine.compile(script).unwrap();

    bench.iter(|| engine.consume_ast(&ast).unwrap());
}

#[bench]
fn bench_eval_switch(bench: &mut Bencher) {
    let script = r#"
        let sum = 0;
        let rem = 0;

        for x in range(0, 10) {
            rem = x % 5;

            sum += switch rem {
                0 => 10,
                1 => 12,
                2 => 42,
                3 => 1,
                _ => 9
            }
        }
    "#;

    let mut engine = Engine::new();
    engine.set_optimization_level(OptimizationLevel::None);

    let ast = engine.compile(script).unwrap();

    bench.iter(|| engine.consume_ast(&ast).unwrap());
}

#[bench]
fn bench_eval_nested_if(bench: &mut Bencher) {
    let script = r#"
        let sum = 0;
        let rem = 0;

        for x in range(0, 10) {
            rem = x % 5;

            sum += if rem == 0 {
                10
            } else if rem == 1 {
                12
            } else if rem == 2 {
                42
            } else if rem == 3 {
                1
            } else{
                9
            };
        }
    "#;

    let mut engine = Engine::new();
    engine.set_optimization_level(OptimizationLevel::None);

    let ast = engine.compile(script).unwrap();

    bench.iter(|| engine.consume_ast(&ast).unwrap());
}
