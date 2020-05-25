#![cfg(not(feature = "no_object"))]

use crate::any::Dynamic;
use crate::def_package;
use crate::engine::Map;
use crate::module::FuncReturn;
use crate::parser::{ImmutableString, INT};

use crate::stdlib::{
    string::{String, ToString},
    vec::Vec,
};

fn map_get_keys(map: &mut Map) -> FuncReturn<Vec<Dynamic>> {
    Ok(map.iter().map(|(k, _)| k.to_string().into()).collect())
}
fn map_get_values(map: &mut Map) -> FuncReturn<Vec<Dynamic>> {
    Ok(map.iter().map(|(_, v)| v.clone()).collect())
}

#[cfg(not(feature = "no_object"))]
def_package!(crate:BasicMapPackage:"Basic object map utilities.", lib, {
    lib.set_fn_2_mut(
        "has",
        |map: &mut Map, prop: ImmutableString| Ok(map.contains_key(prop.as_str())),
    );
    lib.set_fn_1_mut("len", |map: &mut Map| Ok(map.len() as INT));
    lib.set_fn_1_mut("clear", |map: &mut Map| {
        map.clear();
        Ok(())
    });
    lib.set_fn_2_mut(
        "remove",
        |x: &mut Map, name: ImmutableString| Ok(x.remove(name.as_str()).unwrap_or_else(|| ().into())),
    );
    lib.set_fn_2_mut(
        "mixin",
        |map1: &mut Map, map2: Map| {
            map2.into_iter().for_each(|(key, value)| {
                map1.insert(key, value);
            });
            Ok(())
        },
    );
    lib.set_fn_2(
        "+",
        |mut map1: Map, map2: Map| {
            map2.into_iter().for_each(|(key, value)| {
                map1.insert(key, value);
            });
            Ok(map1)
        },
    );

    // Register map access functions
    #[cfg(not(feature = "no_index"))]
    lib.set_fn_1_mut("keys", map_get_keys);

    #[cfg(not(feature = "no_index"))]
    lib.set_fn_1_mut("values", map_get_values);
});
