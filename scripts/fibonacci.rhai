// This script calculates the n-th Fibonacci number using a really dumb algorithm
// to test the speed of the scripting engine.

const target = 30;

let now = timestamp();

fn fib(n) {
    if n < 2 {
         n
    } else {
        fib(n-1) + fib(n-2)
    }
}

print("Ready... Go!");

let result = fib(target);

print("Fibonacci number #" + target + " = " + result);

print("Finished. Run time = " + now.elapsed() + " seconds.");
