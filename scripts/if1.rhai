let a = 42;
let b = 123;
let x = 999;

if a > b {
    print("a > b");
} else if a < b {
    print("a < b");

    let x = 0;          // this 'x' shadows the global 'x'
    print(x);           // should print 0
} else {
    print("a == b");
}