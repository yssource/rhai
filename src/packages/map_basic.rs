use super::{reg_binary, reg_binary_mut, reg_unary_mut};

use crate::any::Dynamic;
use crate::def_package;
use crate::engine::Map;
use crate::fn_register::map_dynamic as map;
use crate::parser::INT;

fn map_get_keys(map: &mut Map) -> Vec<Dynamic> {
    map.iter()
        .map(|(k, _)| Dynamic::from_string(k.to_string()))
        .collect::<Vec<_>>()
}
fn map_get_values(map: &mut Map) -> Vec<Dynamic> {
    map.iter().map(|(_, v)| v.clone()).collect::<Vec<_>>()
}

#[cfg(not(feature = "no_object"))]
def_package!(BasicMapPackage:"Basic object map utilities.", lib, {
    reg_binary_mut(
        lib,
        "has",
        |map: &mut Map, prop: String| map.contains_key(&prop),
        map,
    );
    reg_unary_mut(lib, "len", |map: &mut Map| map.len() as INT, map);
    reg_unary_mut(lib, "clear", |map: &mut Map| map.clear(), map);
    reg_binary_mut(
        lib,
        "remove",
        |x: &mut Map, name: String| x.remove(&name).unwrap_or_else(|| Dynamic::from_unit()),
        map,
    );
    reg_binary_mut(
        lib,
        "mixin",
        |map1: &mut Map, map2: Map| {
            map2.into_iter().for_each(|(key, value)| {
                map1.insert(key, value);
            });
        },
        map,
    );
    reg_binary(
        lib,
        "+",
        |mut map1: Map, map2: Map| {
            map2.into_iter().for_each(|(key, value)| {
                map1.insert(key, value);
            });
            map1
        },
        map,
    );

    // Register map access functions
    #[cfg(not(feature = "no_index"))]
    reg_unary_mut(lib, "keys", map_get_keys, map);

    #[cfg(not(feature = "no_index"))]
    reg_unary_mut(lib, "values", map_get_values, map);
});
