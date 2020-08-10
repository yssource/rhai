use rhai::module_resolvers::*;
use rhai::{Array, Engine, EvalAltResult, Module, RegisterFn, FLOAT};

pub mod raw_fn {
    use rhai::plugin::*;
    use rhai::FLOAT;

    #[export_fn]
    pub fn distance_function(x1: FLOAT, y1: FLOAT, x2: FLOAT, y2: FLOAT) -> FLOAT {
        ((y2 - y1).abs().powf(2.0) + (x2 - x1).abs().powf(2.0)).sqrt()
    }
}

#[test]
fn raw_fn_test() -> Result<(), Box<EvalAltResult>> {
    let mut engine = Engine::new();
    engine.register_fn("get_mystic_number", || 42 as FLOAT);
    let mut m = Module::new();
    rhai::register_exported_fn!(
        m,
        "euclidean_distance".to_string(),
        raw_fn::distance_function
    );
    let mut r = StaticModuleResolver::new();
    r.insert("Math::Advanced".to_string(), m);
    engine.set_module_resolver(Some(r));

    assert_eq!(
        engine.eval::<FLOAT>(
            r#"import "Math::Advanced" as math;
           let x = math::euclidean_distance(0.0, 1.0, 0.0, get_mystic_number()); x"#
        )?,
        41.0
    );
    Ok(())
}

mod raw_fn_mut {
    use rhai::plugin::*;
    use rhai::FLOAT;

    #[export_fn]
    pub fn add_in_place(f1: &mut FLOAT, f2: FLOAT) {
        *f1 += f2;
    }
}

#[test]
fn raw_fn_mut_test() -> Result<(), Box<EvalAltResult>> {
    let mut engine = Engine::new();
    engine.register_fn("get_mystic_number", || 42 as FLOAT);
    let mut m = Module::new();
    rhai::register_exported_fn!(m, "add_in_place", raw_fn_mut::add_in_place);
    let mut r = StaticModuleResolver::new();
    r.insert("Math::Advanced".to_string(), m);
    engine.set_module_resolver(Some(r));

    assert_eq!(
        engine.eval::<FLOAT>(
            r#"import "Math::Advanced" as math;
           let x = get_mystic_number();
           math::add_in_place(x, 1.0);
           x"#
        )?,
        43.0
    );
    Ok(())
}

mod raw_fn_str {
    use rhai::plugin::*;

    #[export_fn]
    pub fn write_out_str(message: &str) -> bool {
        eprintln!("{}", message);
        true
    }
}

#[test]
fn raw_fn_str_test() -> Result<(), Box<EvalAltResult>> {
    let mut engine = Engine::new();
    engine.register_fn("get_mystic_number", || 42 as FLOAT);
    let mut m = Module::new();
    rhai::register_exported_fn!(m, "write_out_str", raw_fn_str::write_out_str);
    let mut r = StaticModuleResolver::new();
    r.insert("Host::IO".to_string(), m);
    engine.set_module_resolver(Some(r));

    assert_eq!(
        engine.eval::<bool>(
            r#"import "Host::IO" as io;
           let x = io::write_out_str("hello world!");
           x"#
        )?,
        true
    );
    Ok(())
}

mod mut_opaque_ref {
    use rhai::plugin::*;
    use rhai::INT;

    #[derive(Clone)]
    pub struct StatusMessage {
        os_code: Option<INT>,
        message: String,
        is_ok: bool,
    }

    #[export_fn]
    pub fn new_message(is_ok: bool, message: &str) -> StatusMessage {
        StatusMessage {
            is_ok,
            os_code: None,
            message: message.to_string(),
        }
    }

    #[export_fn]
    pub fn new_os_message(is_ok: bool, os_code: INT) -> StatusMessage {
        StatusMessage {
            is_ok,
            os_code: Some(os_code),
            message: format!("OS Code {}", os_code),
        }
    }

    #[export_fn]
    pub fn write_out_message(message: &mut StatusMessage) -> bool {
        eprintln!("{}", message.message);
        true
    }
}

#[test]
fn mut_opaque_ref_test() -> Result<(), Box<EvalAltResult>> {
    let mut engine = Engine::new();
    let mut m = Module::new();
    rhai::register_exported_fn!(m, "new_message", mut_opaque_ref::new_message);
    rhai::register_exported_fn!(m, "new_os_message", mut_opaque_ref::new_os_message);
    rhai::register_exported_fn!(m, "write_out_message", mut_opaque_ref::write_out_message);
    let mut r = StaticModuleResolver::new();
    r.insert("Host::Msg".to_string(), m);
    engine.set_module_resolver(Some(r));

    assert_eq!(
        engine.eval::<bool>(
            r#"import "Host::Msg" as msg;
           let message1 = msg::new_message(true, "it worked");
           let ok1 = msg::write_out_message(message1);
           let message2 = msg::new_os_message(true, 0);
           let ok2 = msg::write_out_message(message2);
           ok1 && ok2"#
        )?,
        true
    );
    Ok(())
}

mod rename_fn {
    use rhai::plugin::*;
    use rhai::FLOAT;

    #[export_fn(name = "add_float")]
    pub fn add(f1: FLOAT, f2: FLOAT) -> FLOAT {
        f1 + f2
    }
}

#[test]
fn rename_fn_test() -> Result<(), Box<EvalAltResult>> {
    let mut engine = Engine::new();
    engine.register_fn("get_mystic_number", || 42 as FLOAT);
    let mut m = Module::new();
    rhai::register_exported_fn!(m, "add_two_floats", rename_fn::add_float);
    let mut r = StaticModuleResolver::new();
    r.insert("Math::Advanced".to_string(), m);
    engine.set_module_resolver(Some(r));

    assert_eq!(
        engine.eval::<FLOAT>(
            r#"import "Math::Advanced" as math;
           let x = get_mystic_number();
           let y = math::add_two_floats(x, 1.0);
           y"#
        )?,
        43.0
    );
    Ok(())
}

mod duplicate_fn_rename {
    use rhai::plugin::*;
    use rhai::{FLOAT, INT};

    #[export_fn(name = "add_float")]
    pub fn add(f1: FLOAT, f2: FLOAT) -> FLOAT {
        f1 + f2
    }

    #[export_fn(name = "add_int")]
    pub fn add(i1: INT, i2: INT) -> INT {
        i1 + i2
    }
}

#[test]
fn duplicate_fn_rename_test() -> Result<(), Box<EvalAltResult>> {
    let mut engine = Engine::new();
    engine.register_fn("get_mystic_number", || 42 as FLOAT);
    let mut m = Module::new();
    rhai::register_exported_fn!(m, "add_two_floats", duplicate_fn_rename::add_float);
    rhai::register_exported_fn!(m, "add_two_ints", duplicate_fn_rename::add_int);
    let mut r = StaticModuleResolver::new();
    r.insert("Math::Advanced".to_string(), m);
    engine.set_module_resolver(Some(r));

    let output_array = engine.eval::<Array>(
        r#"import "Math::Advanced" as math;
       let fx = get_mystic_number();
       let fy = math::add_two_floats(fx, 1.0);
       let ix = 42;
       let iy = math::add_two_ints(ix, 1);
       [fy, iy]
       "#
    )?;
    assert_eq!(&output_array[0].as_float().unwrap(), &43.0);
    assert_eq!(&output_array[1].as_int().unwrap(), &43);
    Ok(())
}

pub mod raw_returning_fn {
    use rhai::plugin::*;
    use rhai::FLOAT;

    #[export_fn(return_raw)]
    pub fn distance_function(
        x1: FLOAT,
        y1: FLOAT,
        x2: FLOAT,
        y2: FLOAT,
    ) -> Result<rhai::Dynamic, Box<rhai::EvalAltResult>> {
        Ok(Dynamic::from(
            ((y2 - y1).abs().powf(2.0) + (x2 - x1).abs().powf(2.0)).sqrt(),
        ))
    }
}

#[test]
fn raw_returning_fn_test() -> Result<(), Box<EvalAltResult>> {
    let mut engine = Engine::new();
    engine.register_fn("get_mystic_number", || 42 as FLOAT);
    let mut m = Module::new();
    rhai::register_exported_fn!(
        m,
        "euclidean_distance".to_string(),
        raw_returning_fn::distance_function
    );
    let mut r = StaticModuleResolver::new();
    r.insert("Math::Advanced".to_string(), m);
    engine.set_module_resolver(Some(r));

    assert_eq!(
        engine.eval::<FLOAT>(
            r#"import "Math::Advanced" as math;
           let x = math::euclidean_distance(0.0, 1.0, 0.0, get_mystic_number()); x"#
        )?,
        41.0
    );
    Ok(())
}
