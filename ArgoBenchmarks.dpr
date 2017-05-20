program ArgoBenchmarks;

{$APPTYPE CONSOLE}

uses
  SysUtils, Classes, WinProcs,
  superobject in 'lib\superobject\superobject.pas',
  Argo in 'lib\Argo\Argo.pas',
  ArgoTypes in 'lib\Argo\ArgoTypes.pas';

type
  TProc = reference to procedure;

var
  keys: TStringList;
  
procedure Benchmark(title: String; callback: TProc);
var
  time: Cardinal;
begin
  WriteLn(title);
  time := GetTickCount;
  callback();
  WriteLn(Format('  Completed in %.3fs'#13, [(GetTickCount - time) / 1000.0]));
end;

function LoadString(filename: string): String;
var
  sl: TStringList;
begin
  sl := TStringList.Create;
  try
    sl.LoadFromFile(filename);
    Result := sl.Text;
  finally
    sl.Free;
  end;
end;

function RandomChar: Char;
var
  n: Integer;
begin
  n := Random(62);
  if n < 10 then
    Result := Chr(ord('0') + n)
  else if n < 36 then
    Result := Chr(ord('A') + n - 10)
  else
    Result := Chr(ord('a') + n - 36);
end;

function RandomString(len: Integer = 8): String;
var
  i: Integer;
begin
  Result := '';
  for i := 0 to len - 1 do
    Result := Result + RandomChar;
end;

procedure RandomStringList(strLen: Integer; count: Integer; var sl: TStringList);
var
  i: Integer;
begin
  for i := 0 to count - 1 do
    sl.Add(RandomString(strLen));
end;

procedure CreateKeys;
begin
  keys := TStringList.Create;
  RandomStringList(8, 10000, keys);
end;

procedure BenchmarkStringList;
var
  sl: TStringList;
begin
  WriteLn('== TStringList ==');

  Benchmark('Creating 10,000 strings', procedure
    var
      i: Integer;
    begin
      sl := TStringList.Create;
      for i := 0 to Pred(keys.Count) do
        sl.Add(keys[i]);
    end);

  Benchmark('Finding 5,000 string indexes', procedure
    var
      i: Integer;
    begin
      for i := 0 to 4999 do
        sl.IndexOf(keys[i]);
    end);

  Benchmark('Accessing 100,000 strings', procedure
    var
      i: Integer;
    begin
      for i := 0 to 99999 do
        sl[i mod 10000];
    end);

  sl.Free;
  WriteLn(' ');
end;

procedure BenchmarkFastStringList;
var
  sl: TFastStringList;
begin
  WriteLn('== TFastStringList ==');

  Benchmark('Creating 10,000 strings', procedure
    var
      i: Integer;
    begin
      sl := TFastStringList.Create;
      for i := 0 to Pred(keys.Count) do
        sl.Add(keys[i]);
    end);

  Benchmark('Finding 5,000 string indexes', procedure
    var
      i: Integer;
    begin
      for i := 0 to 4999 do
        sl.IndexOf(keys[i]);
    end);

  Benchmark('Accessing 100,000 strings', procedure
    var
      i: Integer;
    begin
      for i := 0 to 99999 do
        sl[i mod 10000];
    end);

  sl.Free;
  WriteLn(' ');
end;

procedure BenchmarkArgoTree;
var
  tree: TArgoTree;
begin
  WriteLn('== TArgoTree ==');

  Benchmark('Creating 10,000 strings', procedure
    var
      i: Integer;
    begin
      tree := TArgoTree.Create;
      for i := 0 to Pred(keys.Count) do
        tree.Add(keys[i]);
    end);

  Benchmark('Finding 5,000 string indexes', procedure
    var
      i: Integer;
    begin
      for i := 0 to 4999 do
        tree[keys[i]];
    end);

  Benchmark('Accessing 100,000 strings', procedure
    var
      i: Integer;
    begin
      for i := 0 to 99999 do
        tree.Names[i mod 10000];
    end);

  WriteLn(' ');
end;

procedure BenchmarkDeserialization;
var
  json: String;
  obj: TJSONObject;
begin
  WriteLn('== DESERIALIZATION ==');

  // 4,286 bytes
  json := LoadString('xtest-2.esp.json');
  Benchmark('xtest-2.esp.json x500', procedure
    var
      i: Integer;
    begin
      for i := 1 to 500 do
        obj := TJSONObject.Create(json);
    end);

  // 4,909,700 bytes
  json := LoadString('Update.esm.json');
  Benchmark('Update.esm.json', procedure
    begin
      obj := TJSONObject.Create(json);
    end);

  // 11,353,142 bytes
  json := LoadString('HearthFires.esm.json');
  Benchmark('HearthFires.esm.json', procedure
    begin
      obj := TJSONObject.Create(json);
    end);

  WriteLn(' ');
end;

procedure BenchmarkDeserializationSO;
var
  json: String;
  obj: ISuperObject;
begin
  WriteLn('== SUPEROBJECT DESERIALIZATION ==');

  // 4,286 bytes
  json := LoadString('xtest-2.esp.json');
  Benchmark('xtest-2.esp.json x500', procedure
    var
      i: Integer;
    begin
      for i := 1 to 500 do
        obj := SO(json);
    end);

  // 4,909,700 bytes
  json := LoadString('Update.esm.json');
  Benchmark('Update.esm.json', procedure
    begin
      obj := SO(json);
    end);

  // 11,353,142 bytes
  json := LoadString('HearthFires.esm.json');
  Benchmark('HearthFires.esm.json', procedure
    begin
      obj := SO(json);
    end);

  WriteLn(' ');
end;

procedure BenchmarkSerialization;
var
  json: string;
  obj: TJSONObject;
begin
  WriteLn('== SERIALIZATION ==');

  // 4,286 bytes
  json := LoadString('xtest-2.esp.json');
  obj := TJSONObject.Create(json);
  Benchmark('xtest-2.esp.json x500', procedure
    var
      i: Integer;
    begin
      for i := 1 to 500 do
        json := obj.ToString;
    end);

  // 4,909,700 bytes
  json := LoadString('Update.esm.json');
  obj := TJSONObject.Create(json);
  Benchmark('Update.esm.json', procedure
    begin
      json := obj.ToString;
    end);

  // 11,353,142 bytes
  json := LoadString('HearthFires.esm.json');
  obj := TJSONObject.Create(json);
  Benchmark('HearthFires.esm.json', procedure
    begin
      json := obj.ToString;
    end);

  WriteLn(' ');
end;

procedure BenchmarkSerializationSO;
var
  json: string;
  obj: ISuperObject;
begin
  WriteLn('== SUPEROBJECT SERIALIZATION ==');

  // 4,286 bytes
  json := LoadString('xtest-2.esp.json');
  obj := SO(json);
  Benchmark('xtest-2.esp.json x500', procedure
    var
      i: Integer;
    begin
      for i := 1 to 500 do
        json := obj.ASJSon;
    end);

  // 4,909,700 bytes
  json := LoadString('Update.esm.json');
  obj := SO(json);
  Benchmark('Update.esm.json', procedure
    begin
      json := obj.ASJSon;
    end);

  // 11,353,142 bytes
  json := LoadString('HearthFires.esm.json');
  obj := SO(json);
  Benchmark('HearthFires.esm.json', procedure
    begin
      json := obj.ASJSon;
    end);

  WriteLn(' ');
end;

procedure BenchmarkAccess;
var
  obj: TJSONObject;
begin
  WriteLn('== ACCESS ==');

  obj := TJSONObject.Create;
  Benchmark('Creating 10,000 top-level integer properties', procedure
    var
      i: Integer;
    begin
      for i := 0 to 9999 do
        obj.I[keys[i]] := Random(2147483647);
    end);

  Benchmark('Accessing 100,000 top-level integer properties', procedure
    var
      i: Integer;
    begin
      for i := 0 to 99999 do
        obj.I[keys[i mod 10000]];
    end);

  WriteLn(' ');
end;

procedure BenchmarkAccessSO;
var
  obj: ISuperObject;
begin
  WriteLn('== SUPEROBJECT ACCESS ==');

  obj := SO;
  Benchmark('Creating 10,000 top-level integer properties', procedure
    var
      i: Integer;
    begin
      for i := 0 to 9999 do
        obj.I[keys[i]] := Random(2147483647);
    end);

  Benchmark('Accessing 100,000 top-level integer properties', procedure
    var
      i: Integer;
    begin
      for i := 0 to 99999 do
        obj.I[keys[i mod 10000]];
    end);
end;

begin
  try
    CreateKeys;
    BenchmarkStringList;
    BenchmarkFastStringList;
    BenchmarkArgoTree;
    BenchmarkDeserialization;
    BenchmarkDeserializationSO;
    BenchmarkSerialization;
    BenchmarkSerializationSO;
    BenchmarkAccess;
    BenchmarkAccessSO;
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
  ReadLn;
end.
