#![cfg(not(feature = "no_object"))]

use crate::engine::OP_EQUALS;
use crate::plugin::*;
use crate::{def_package, Dynamic, ImmutableString, Map, INT};

#[cfg(not(feature = "no_index"))]
use crate::Array;

def_package!(crate:BasicMapPackage:"Basic object map utilities.", lib, {
    combine_with_exported_module!(lib, "map", map_functions);
});

#[export_module]
mod map_functions {
    pub fn has(map: &mut Map, prop: ImmutableString) -> bool {
        map.contains_key(&prop)
    }
    pub fn len(map: &mut Map) -> INT {
        map.len() as INT
    }
    pub fn clear(map: &mut Map) {
        map.clear();
    }
    pub fn remove(x: &mut Map, name: ImmutableString) -> Dynamic {
        x.remove(&name).unwrap_or_else(|| ().into())
    }
    #[rhai_fn(name = "mixin", name = "+=")]
    pub fn mixin(map1: &mut Map, map2: Map) {
        map2.into_iter().for_each(|(key, value)| {
            map1.insert(key, value);
        });
    }
    #[rhai_fn(name = "+")]
    pub fn merge(mut map1: Map, map2: Map) -> Map {
        map2.into_iter().for_each(|(key, value)| {
            map1.insert(key, value);
        });
        map1
    }
    pub fn fill_with(map1: &mut Map, map2: Map) {
        map2.into_iter().for_each(|(key, value)| {
            map1.entry(key).or_insert(value);
        });
    }
    #[rhai_fn(name = "==", return_raw)]
    pub fn equals(
        mut ctx: NativeCallContext,
        map1: &mut Map,
        mut map2: Map,
    ) -> Result<Dynamic, Box<EvalAltResult>> {
        if map1.len() != map2.len() {
            return Ok(false.into());
        }
        if map1.is_empty() {
            return Ok(true.into());
        }

        let def_value = Some(false.into());

        for (m1, v1) in map1.iter_mut() {
            if let Some(v2) = map2.get_mut(m1) {
                let equals = ctx
                    .call_fn_dynamic_raw(OP_EQUALS, true, false, &mut [v1, v2], def_value.clone())
                    .map(|v| v.as_bool().unwrap_or(false))?;

                if !equals {
                    return Ok(false.into());
                }
            } else {
                return Ok(false.into());
            }
        }

        Ok(true.into())
    }
    #[rhai_fn(name = "!=", return_raw)]
    pub fn not_equals(
        ctx: NativeCallContext,
        map1: &mut Map,
        map2: Map,
    ) -> Result<Dynamic, Box<EvalAltResult>> {
        equals(ctx, map1, map2).map(|r| (!r.as_bool().unwrap()).into())
    }

    #[cfg(not(feature = "no_index"))]
    pub mod indexing {
        pub fn keys(map: &mut Map) -> Array {
            map.iter().map(|(k, _)| k.clone().into()).collect()
        }
        pub fn values(map: &mut Map) -> Array {
            map.iter().map(|(_, v)| v.clone()).collect()
        }
    }
}
