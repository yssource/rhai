Disable Custom Types
====================

{{#include ../links.md}}

The custom types API `register_type`, `register_type_with_name`, `register_get`, `register_set`, `register_get_set`,
`register_indexer_get`, `register_indexer_set` and `register_indexer_get_set` are not available under [`no_object`].

The indexers API `register_indexer_get`, `register_indexer_set` and `register_indexer_get_set` are also
not available under [`no_index`].
