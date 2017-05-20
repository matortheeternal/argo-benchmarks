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

|                                                | Argo   | superobject |
|------------------------------------------------|--------|-------------|
| Deserialization of xtest-2.esp.json x500       | 0.094s | 0.063s      |
| Deserialization of Update.esm.json             | 0.218s | 0.140s      |
| Deserialization of HearthFires.esm.json        | 0.514s | 0.343s      |
| Serialization of xtest-2.esp.json x500         | 0.031s | 0.047s      |
| Serialization of Update.esm.json               | 0.078s | 0.109s      |
| Serialization of HearthFires.esm.json          | 0.218s | 0.265s      |
| Creating 10,000 top-level integer properties   | 0.000s | 0.015s      |
| Accessing 100,000 top-level integer properties | 0.032s | 0.203s      |
