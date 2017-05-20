# argo-benchmarks
Benchmarks for [argo](https://github.com/matortheeternal/argo).

## results

Benchmark results as of 5/19/2017.

### data types

|                 | Creating 10,000 strings | Finding 5,000 string indexes | Accessing 10,000 strings (by index) |
|-----------------|-------------------------|------------------------------|-------------------------------------|
| TStringList     | 0.000s                  | 1.560s                       | 0.000s                              |
| TFastStringList | 0.000s                  | 0.296s                       | 0.000s                              |
| TArgoTree       | 0.000s                  | 0.000s                       | 4.165s                              |

### json

|                                                | Argo time | Argo memory | superobject time | superobject memory |
|------------------------------------------------|-----------|-------------|------------------|--------------------|
| Deserialization of xtest-2.esp.json x500       | 0.094s    | 16.462mb    | 0.063s           | 14.352mb           |
| Deserialization of Update.esm.json             | 0.218s    | 55.710mb    | 0.140s           | 40.380mb           |
| Deserialization of HearthFires.esm.json        | 0.500s    | 146.145mb   | 0.343s           | 101.042mb          |
| Serialization of xtest-2.esp.json x500         | 0.016s    | 0.851mb     | 0.047s           | 0.847mb            |
| Serialization of Update.esm.json               | 0.078s    | 45.794mb    | 0.109s           | 40.399mb           |
| Serialization of HearthFires.esm.json          | 0.218s    | 126.864mb   | 0.265s           | 121.892mb          |
| Creating 10,000 top-level integer properties   | 0.015s    | 1.316mb     | 0.047s           | 1.841mb            |
| Accessing 100,000 top-level integer properties | 0.062s    | 1.316mb     | 0.250s           | 1.841mb            |
