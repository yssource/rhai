Singleton Command Object
=======================

{{#include ../links.md}}


Usage Scenario
--------------

* A system provides core functionalities, but no driving logic.

* The driving logic must be dynamic and hot-loadable.

* A script is used to drive the system and provide control intelligence.

* The API is multiplexed, meaning that it can act on multiple system-provided entities, or

* The API lends itself readily to an object-oriented (OO) representation.


Key Concepts
------------

* Expose a Command type with an API.  The [`no_object`] feature must not be on.

* Leverage [function overloading] to simplify the API design.

* Since Rhai is _[sand-boxed]_, it cannot mutate the environment.  To perform external actions via an API, the command object type must be wrapped in a `RefCell` (or `RwLock`/`Mutex` for [`sync`]) and shared to the [`Engine`].

* Load each command object into a custom [`Scope`] as constant variables.

* Control each command object in script via the constants.


Implementation
--------------

There are two broad ways for Rhai to control an external system, both of which involve
wrapping the system in a shared, interior-mutated object.

This is the other way which involves directly exposing the data structures of the external system
as a name singleton object in the scripting space.

Use this when the API is complex and clearly object-based.

For a relatively simple API that is action-based and not object-based,
use the [Control Layer]({{rootUrl}}/patterns/control.md) pattern instead.


### Functional API

Assume the following command object type:

```rust
struct EnergizerBunny { ... }

impl EnergizerBunny {
    pub fn new () -> Self { ... }
    pub fn go (&mut self) { ... }
    pub fn stop (&mut self) { ... }
    pub fn is_going (&self) { ... }
    pub fn get_speed (&self) -> i64 { ... }
    pub fn set_speed (&mut self, speed: i64) { ... }
    pub fn turn (&mut self, left_turn: bool) { ... }
}
```

### Wrap Command Object Type as Shared

```rust
let SharedBunnyType = Rc<RefCell<EnergizerBunny>>;
```

### Register the Custom Type

```rust
engine.register_type_with_name::<SharedBunnyType>("EnergizerBunny");
```

### Register Methods and Getters/Setters

```rust
engine
    .register_get_set("power",
        |bunny: &mut SharedBunnyType| bunny.borrow().is_going(),
        |bunny: &mut SharedBunnyType, on: bool| {
            if on {
                if bunny.borrow().is_going() {
                    println!("Still going...");
                } else {
                    bunny.borrow_mut().go();
                }
            } else {
                if bunny.borrow().is_going() {
                    bunny.borrow_mut().stop();
                } else {
                    println!("Already out of battery!");
                }
            }
        }
    ).register_get("speed", |bunny: &mut SharedBunnyType| {
        if bunny.borrow().is_going() {
            bunny.borrow().get_speed()
        } else {
            0
        }
    }).register_set_result("speed", |bunny: &mut SharedBunnyType, speed: i64| {
        if speed <= 0 {
            Err("Speed must be positive!".into())
        } else if speed > 100 {
            Err("Bunny will be going too fast!".into())
        } else if !bunny.borrow().is_going() {
            Err("Bunny is not yet going!".into())
        } else {
            b.borrow_mut().set_speed(speed);
            Ok(().into())
        }
    }).register_fn("turn_left", |bunny: &mut SharedBunnyType| {
        if bunny.borrow().is_going() {
            bunny.borrow_mut().turn(true);
        }
    }).register_fn("turn_right", |bunny: &mut SharedBunnyType| {
        if bunny.borrow().is_going() {
            bunny.borrow_mut().turn(false);
        }
    });
```

### Push Constant Command Object into Custom Scope

```rust
let bunny: SharedBunnyType = Rc::new(RefCell::(EnergizerBunny::new()));

let mut scope = Scope::new();
scope.push_constant("BUNNY", bunny.clone());

engine.consume_with_scope(&mut scope, script)?;
```

### Use the Command API in Script

```rust
// Access the command object via constant variable 'BUNNY'.

if !BUNNY.power { BUNNY.power = true; }

if BUNNY.speed > 50 { BUNNY.speed = 50; }

BUNNY.turn_left();
```
