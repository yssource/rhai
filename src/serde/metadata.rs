#![cfg(feature = "metadata")]

use crate::module::calc_native_fn_hash;
use crate::{calc_fn_hash, Engine, AST};
use serde::{Deserialize, Serialize};
#[cfg(feature = "no_std")]
use std::prelude::v1::*;
use std::{cmp::Ordering, collections::BTreeMap, iter::empty};

#[derive(Debug, Clone, Copy, Eq, PartialEq, Hash, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
enum FnType {
    Script,
    Native,
}

#[derive(Debug, Clone, Copy, Eq, PartialEq, Hash, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
enum FnNamespace {
    Global,
    Internal,
}

impl From<crate::FnNamespace> for FnNamespace {
    fn from(value: crate::FnNamespace) -> Self {
        match value {
            crate::FnNamespace::Global => Self::Global,
            crate::FnNamespace::Internal => Self::Internal,
        }
    }
}

#[derive(Debug, Clone, Copy, Eq, PartialEq, Hash, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
enum FnAccess {
    Public,
    Private,
}

impl From<crate::FnAccess> for FnAccess {
    fn from(value: crate::FnAccess) -> Self {
        match value {
            crate::FnAccess::Public => Self::Public,
            crate::FnAccess::Private => Self::Private,
        }
    }
}

#[derive(Debug, Clone, Eq, PartialEq, Hash, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
struct FnParam<'a> {
    #[serde(rename = "type", skip_serializing_if = "Option::is_none")]
    pub name: Option<&'a str>,
    #[serde(rename = "type", skip_serializing_if = "Option::is_none")]
    pub typ: Option<&'a str>,
}

impl PartialOrd for FnParam<'_> {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(match self.name.partial_cmp(&other.name).expect("succeed") {
            Ordering::Less => Ordering::Less,
            Ordering::Greater => Ordering::Greater,
            Ordering::Equal => self.typ.partial_cmp(&other.typ).expect("succeed"),
        })
    }
}

impl Ord for FnParam<'_> {
    fn cmp(&self, other: &Self) -> Ordering {
        match self.name.cmp(&other.name) {
            Ordering::Equal => self.typ.cmp(&other.typ),
            cmp => cmp,
        }
    }
}

#[derive(Debug, Clone, Eq, PartialEq, Hash, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
struct FnMetadata<'a> {
    pub base_hash: u64,
    pub full_hash: u64,
    pub namespace: FnNamespace,
    pub access: FnAccess,
    pub name: String,
    #[serde(rename = "type")]
    pub typ: FnType,
    pub num_params: usize,
    #[serde(default, skip_serializing_if = "Vec::is_empty")]
    pub params: Vec<FnParam<'a>>,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub return_type_name: Option<&'a str>,
    pub signature: String,
    #[serde(default, skip_serializing_if = "Vec::is_empty")]
    pub doc_comments: Vec<&'a str>,
}

impl PartialOrd for FnMetadata<'_> {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for FnMetadata<'_> {
    fn cmp(&self, other: &Self) -> Ordering {
        match self.name.cmp(&other.name) {
            Ordering::Equal => match self.num_params.cmp(&other.num_params) {
                Ordering::Equal => self.params.cmp(&other.params),
                cmp => cmp,
            },
            cmp => cmp,
        }
    }
}

impl<'a> From<&'a crate::module::FuncInfo> for FnMetadata<'a> {
    fn from(info: &'a crate::module::FuncInfo) -> Self {
        let base_hash = calc_fn_hash(&info.name, info.params);
        let (typ, full_hash) = if info.func.is_script() {
            (FnType::Script, base_hash)
        } else {
            (
                FnType::Native,
                calc_native_fn_hash(empty::<&str>(), &info.name, &info.param_types),
            )
        };

        Self {
            base_hash,
            full_hash,
            namespace: info.namespace.into(),
            access: info.access.into(),
            name: info.name.to_string(),
            typ,
            num_params: info.params,
            params: info
                .param_names_and_types
                .iter()
                .map(|s| {
                    let mut seg = s.splitn(2, ':');
                    let name = match seg.next().map(&str::trim).unwrap_or("_") {
                        "_" => None,
                        s => Some(s),
                    };
                    let typ = seg.next().map(&str::trim);
                    FnParam { name, typ }
                })
                .collect(),
            return_type_name: match info.return_type_name.as_str() {
                "" | "()" => None,
                ty => Some(ty),
            },
            signature: info.gen_signature(),
            doc_comments: if info.func.is_script() {
                #[cfg(feature = "no_function")]
                unreachable!("script-defined functions should not exist under no_function");

                #[cfg(not(feature = "no_function"))]
                info.func
                    .get_script_fn_def()
                    .expect("script-defined function")
                    .comments
                    .as_ref()
                    .map_or_else(|| Vec::new(), |v| v.iter().map(|s| &**s).collect())
            } else {
                #[cfg(not(feature = "metadata"))]
                {
                    Vec::new()
                }
                #[cfg(feature = "metadata")]
                {
                    info.comments
                        .as_ref()
                        .map_or_else(|| Vec::new(), |v| v.iter().map(|s| &**s).collect())
                }
            },
        }
    }
}

#[derive(Debug, Clone, Serialize)]
#[serde(rename_all = "camelCase")]
struct ModuleMetadata<'a> {
    #[serde(skip_serializing_if = "BTreeMap::is_empty")]
    pub modules: BTreeMap<&'a str, Self>,
    #[serde(skip_serializing_if = "Vec::is_empty")]
    pub functions: Vec<FnMetadata<'a>>,
}

impl ModuleMetadata<'_> {
    #[inline(always)]
    pub fn new() -> Self {
        Self {
            modules: BTreeMap::new(),
            functions: Vec::new(),
        }
    }
}

impl<'a> From<&'a crate::Module> for ModuleMetadata<'a> {
    fn from(module: &'a crate::Module) -> Self {
        let mut functions: Vec<_> = module.iter_fn().map(|f| f.into()).collect();
        functions.sort();

        Self {
            modules: module
                .iter_sub_modules()
                .map(|(name, m)| (name, m.as_ref().into()))
                .collect(),
            functions,
        }
    }
}

#[cfg(feature = "metadata")]
impl Engine {
    /// _(metadata)_ Generate a list of all functions (including those defined in an
    /// [`AST`][crate::AST]) in JSON format.
    /// Exported under the `metadata` feature only.
    ///
    /// Functions from the following sources are included:
    /// 1) Functions defined in an [`AST`][crate::AST]
    /// 2) Functions registered into the global namespace
    /// 3) Functions in static modules
    /// 4) Functions in global modules (optional)
    pub fn gen_fn_metadata_with_ast_to_json(
        &self,
        ast: &AST,
        include_global: bool,
    ) -> serde_json::Result<String> {
        let _ast = ast;
        let mut global = ModuleMetadata::new();

        self.global_sub_modules.iter().for_each(|(name, m)| {
            global.modules.insert(name, m.as_ref().into());
        });

        self.global_modules
            .iter()
            .take(if include_global { usize::MAX } else { 1 })
            .flat_map(|m| m.iter_fn())
            .for_each(|f| global.functions.push(f.into()));

        #[cfg(not(feature = "no_function"))]
        _ast.shared_lib()
            .iter_fn()
            .for_each(|f| global.functions.push(f.into()));

        global.functions.sort();

        serde_json::to_string_pretty(&global)
    }

    /// Generate a list of all functions in JSON format.
    /// Exported under the `metadata` feature only.
    ///
    /// Functions from the following sources are included:
    /// 1) Functions registered into the global namespace
    /// 2) Functions in static modules
    /// 3) Functions in global modules (optional)
    #[inline(always)]
    pub fn gen_fn_metadata_to_json(&self, include_global: bool) -> serde_json::Result<String> {
        self.gen_fn_metadata_with_ast_to_json(&AST::empty(), include_global)
    }
}
