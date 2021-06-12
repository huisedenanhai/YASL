# Definitions

## Syntax

```
t :=    AtomicValues            (T-Atom)
        fn name: PlainType. t   (T-Abs)
        t t                     (T-App)
        let id = t in t         (T-Let)
        if t then t else t      (T-If)
        loop t in t             (T-Loop)
        [t, ..., t]             (T-Tuple)
        TyName[t, ..., t]       (T-NamedTuple)
        t.IntLiteral            (T-TupleAccess)
        TyName{li = t, ..., lj = t} (T-Record)
        t.l                     (T-RecordAccess)
        -t                      (T-Minus)
        t op t                  (T-BinaryOp)
        name                    (T-Ident)
    
AtomicValues := IntLiteral
                FloatLiteral
                BoolLiteral

v :=    AtomicValues
        TyName{li = v, ..., lj = v}
        [v, ..., v]
        TyName[v, ..., v]

T :=    PlainType
        PlainType <'c>-> T

PlainType :=    TyName
                [PlainType, ..., PlainType]

TypeDeclare :=  type TyName[PlainType, ... , PlainType]
                type Tyname{li: PlainType, ..., lj: PlainType}
                type Tyname;

TopLevel := uniform name: PlainType
            extern func_name: PlainType -> T 
            extern func_name: PlainType -> T = native_name
            TypDeclare
            let name = t
```

`'c` in function type `PlainType <'c>-> T` is auto generated function id, so each function has different type, which makes code gen simple, as calling context and behaviour are all deterministic for a function.

Users will not write any `'c`. If one writes `T1 -> T2` for type checking, he declares a template type with parameter `'c`, which will be instantiated later as `T1 <'c>-> T2`.

We currently do not allow function as argument to avoid complex generic types.

Types should be forward declared. We don't support recursive types.

Operators are overloaded. Operators are not a term by its own. Our syntax also avoid partial application of operators. Thus each terms still has unique types.

Unary and binary operators are merged into TmOp in our implementation for simplicity.

Variable names can be shadowed, while type names can not.

There should be a `main` function with signature `vec2 -> vec4`, which takes a frag uv and return the pixel color.

Currently we only generate fragment shader.

## Typing

```

```

## Translation

```
```