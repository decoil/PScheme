{$mode delphi}

uses character, sysUtils;

procedure fail(s: string);
begin
  writeln(s);
  halt;
end;

var
  nextChar: char = ' ';   // ' ' means no char

// Read a single token into s; return false on end of input.
function lex(out s: string): boolean;
var
  c: char;
begin
  if nextChar <> ' ' then
    begin
      s := nextChar;
      nextChar := ' ';
    end
  else
    begin
      if seekEof then exit(false);
      read(c);
      s := c;
    end;
  
  if (s <> '(') and (s <> ')') then
    while not eof do
      begin
        read(c);
        if c = ')' then
          begin
            nextChar := c;  // push the character back
            break;
          end;
        if isWhiteSpace(c) then break;
        s += c;
      end;
      
  exit(true);
end;

type
  typ = (int, bool, sym, cons, fun, err);
  
  pcell = ^cell;
  
  val = record
    case t: typ of
      int: (i: integer);
      bool: (b: boolean);
      sym: (s: ^string);
      cons: (p: pcell);
      fun: (f: ^val);
      err: (e: ^string);
  end;
  
  cell = record
    car, cdr: val;
  end;
  
  expr = val;
  
  env = record
    name: string;
    v: val;
    next: ^env;
  end;
  penv = ^env;

function parseList(): expr; forward;

var
  nextToken: string = '';

// Parse an expression, or return false at end of file.
function parse(out e: val): boolean;
var
  t: string;
begin
  if nextToken <> '' then
    begin
      t := nextToken;
      nextToken := '';
    end
  else if not lex(t) then exit(false);
  
  if isdigit(t[1]) then
    begin
      e.t := int;
      e.i := strToInt(t);
    end
  else if (t = '#f') or (t = '#t') then
    begin
      e.t := bool;
      e.b := (t = '#t');
    end
  else if t <> '(' then
    begin
      e.t := sym;
      new(e.s);
      e.s^ := t;
    end
  else  // t =  '('
    e := parseList();
    
  exit(true);
end;

function parseList(): expr;
var
  t: string;
  e: expr;
begin
  if not lex(t) then fail('unexpected end of file');
  e.t := cons;
  
  if t = ')' then
    begin
      e.p := nil;
      exit(e);
    end;
  nextToken := t;  // push the token back

  new(e.p);
  parse(e.p^.car);
  e.p^.cdr := parseList();
  exit(e);
end;

procedure print(v: val);
begin
  case v.t of
    int: write(v.i, ' ');
    bool: write(v.b, ' ');
    sym: write(v.s^, ' ');
    cons:
      if v.p = nil then write('nil')
       else
        begin
          write('(');
          print(v.p^.car);
          write(' . ');
          print(v.p^.cdr);
          write(')');
        end;
    fun: fail('no functions yet');
    err: write('error: ', v.e^);
  end;
end;

procedure println(v: val);
begin
  print(v);
  writeln;
end;

function error(s: string): val;
var
  v: val;
begin
  v.t := err;
  new(v.e);
  v.e^ := s;
  exit(v);
end;

// Return the length of a linked list.
function len(v: val): integer;
begin
  if v.t <> cons then fail('cons expected');
  if v.p = nil then exit(0);
  exit(len(v.p^.cdr) + 1);
end;

// Get the nth element of a linked list.
function nth(v: val; n: integer): val;
begin
  if v.t <> cons then fail('cons expected');
  if v.p = nil then fail('list is too short');
  
  if n = 0 then exit(v.p^.car);
  exit(nth(v.p^.cdr, n - 1));
end;

function head(v: val): val;
begin
  exit(nth(v, 0));
end;

const
  intOps: array[1..5] of string = ('+', '-', '*', '/', '%');

function evalIntOp(op: char; i, j: integer): integer;
begin
  case op of
    '+': exit(i + j);
    '-': exit(i - j);
    '*': exit(i * j);
    '/': exit(i div j);
    '%': exit(i mod j);
    else fail('unknown op');
  end;
end;

const
  boolOps: array[1..4] of string = ('<', '<=', '>', '>=');

function evalBoolOp(op: string; i, j: integer): boolean;
begin
  if op = '<' then exit(i < j);
  if op = '<=' then exit(i <= j);
  if op = '>' then exit(i > j);
  if op = '>=' then exit(i >= j);
  fail('unknown op');
end;

const
  eqOps: array[1..2] of string = ('=', '<>');

function eq(v, w: val): boolean;
begin
  if v.t <> w.t then exit(false);
  case v.t of
    int: exit(v.i = w.i);
    bool: exit(v.b = w.b);
    cons: exit(v.p = w.p);
    else fail('unknown type');
  end;
end;

function evalEqOp(op: string; v, w: val): boolean;
begin
  if op = '=' then exit(eq(v, w));
  if op = '<>' then exit(not eq(v, w));
  fail('unknown op');
end;

const
  predOps: array[1..4] of string = ('int?', 'bool?', 'sym?', 'cons?');

function evalPredOps(op: string; v:val): boolean;
begin
  if op = 'int?' then exit(v.t = int);
  if op = 'bool?' then exit(v.t = bool);
  if op = 'sym?' then exit(v.t = sym);
  if op = 'cons?' then exit(v.t = cons);
end; 


function inArr(s: string; const a: array of string): boolean;
var
  t: string;
begin
  for t in a do
    if s = t then exit(true);
  exit(false);
end;

function eval(v: expr; env: penv): val; forward;

function evalOp(v: val; b: string; env: penv): val;
var
  arg1, arg2, r: val;
begin
  if len(v) <> 3 then exit(error('wrong argument count'));
  arg1 := eval(nth(v, 1), env);
  arg2 := eval(nth(v, 2), env);
  if arg1.t = err then exit(arg1);
  if arg2.t = err then exit(arg2);
  
  // type checking
  
  if inArr(b, intOps) or inArr(b, boolOps) then
      if (arg1.t <> int) or (arg2.t <> int) then exit(error('integer expected'));
  
  if inArr(b, intOps) then
    begin
      r.t := int;
      r.i := evalIntOp(b[1], arg1.i, arg2.i);
    end;
  if inArr(b, boolOps) then
    begin
      r.t := bool;
      r.b := evalBoolOp(b, arg1.i, arg2.i);
    end;
  if inArr(b, eqOps) then
    begin
      r.t := bool;
      r.b := evalEqOp(b, arg1, arg2);
    end;
  exit(r);
end;

const
  consOps: array[1 .. 3] of string = ('cons', 'car', 'cdr');

function evalconsOp(v: val; b: string; env: penv): val;
var
  q, r: val;
begin
  if b = 'cons' then
    begin
      if len(v) <> 3 then exit(error('wrong argument count'));
      r.t := cons;
      new(r.p);
      q := eval(nth(v,1), env);
      if q.t = err then exit(q);
      r.p^.car := q;
      q := eval(nth(v,2), env);
      if q.t = err then exit(q);
      r.p^.cdr := q;
      exit(r);
    end;
   if len(v) <> 2 then exit(error('wrong argument count'));
   
   q := eval(nth(v,1), env);
   if q.t = err then exit(q);
   if q.t <> cons then exit(error('not a cons cell'));
   if q.p = nil then exit(error('car/cdr of nil'));
   if b = 'car' then exit(q.p^.car) else exit(q.p^.cdr);
end;

function evalIf(v: expr; env: penv): val;
var
  cond: expr;
  k: integer;
begin
  if len(v) <> 4 then exit(error('syntax error in if'));
  cond := eval(nth(v, 1), env);
  if cond.t = err then exit(cond);
  if cond.t <> bool then exit(error('if condition must be boolean'));
  if cond.b then k := 2 else k := 3;
  exit(eval(nth(v, k), env));
end;

function evalLet(exp: expr; env: penv): val;
var
  arg, vname, v: val;
  name: string;
  e: ^env;
begin
  if len(exp) <> 3 then exit(error('syntax error in let'));
  arg := nth(exp, 1);
  if (arg.t <> cons) or (len(arg) <> 2) then exit(error('malformed let'));
  vname := head(arg);
  if vname.t <> sym then exit(error('let can only bind variables'));
  name := vname.s^;
  v := eval(nth(arg, 1), env);
  if v.t = err then exit(v);
  
  new(e);
  e^.name := name;
  e^.v := v;
  e.next := env;
  
  v := eval(nth(exp, 2), e);
  dispose(e);
  exit(v);
end;

function evalDef(exp: expr; env: penv): val;
var 
  arg, v: val;
  name: string;
  e :penv;
begin
  if len(exp) <> 3 then exit(error('syntax error in let'));
  arg := nth(exp, 1);
  if (arg.t <> sym) then exit(error('malformed define'));
      
      name := arg.s^;
      v := eval(nth(exp, 2), env);
      if v.t = err then exit(v);
     new(e);
     e^.name := name;
     e^.v := v;
     e.next := env;
     v := eval(nth(exp, 2), env);

     exit(v);
end;


function lookup(name: string; env: penv): val;
begin
  if env = nil then exit(error('variable ' + name + ' not found'));
  if env^.name = name then exit(env^.v);
  
  exit(lookup(name, env^.next));
end;

function eval(v: expr; env: penv): val;
var
  w, arg, r: expr;
  b: string;
begin
  if v.t = sym then
    if v.s^ = 'nil' then
      begin
        r.t := cons;
        r.p := nil;
        exit(r);
      end
    else exit(lookup(v.s^, env));
    
  if v.t <> cons then exit(v);
  if v.p = nil then exit(error('function call is empty'));
  
  w := nth(v, 0);   // get head
  if w.t <> sym then exit(error('can only call builtins'));
  
  b := w.s^;
  if inArr(b, intOps) or inArr(b, boolOps) or inArr(b, eqOps) then
    exit(evalOp(v, b, env));

  if inArr(b, consOps) then
    exit(evalconsOp(v, b, env));

  if inArr(b, predOps) then
    begin
      if len(v) <> 2 then exit(error('predicates must contain one argument'));
      arg := eval(nth(v, 1), env);
      if arg.t = err then exit(arg);
      r.t := bool;
      r.b := evalPredOps(b, arg);
      exit(r);
    end;

  if b = 'if' then exit(evalIf(v, env));
  if b = 'let' then exit(evalLet(v, env));
  if b = 'define' then exit(evalDef(v, env));
  exit(error('unknown function'));
end;

// main

var
  v: val;
begin
writeln('PScheme v.0.0.6');
  while true do
    begin
      write('> ');
      if not parse(v) then break;
      println(eval(v, nil));
    end;
end.
