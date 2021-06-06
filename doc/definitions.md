# Definitions

## Syntax

```
t :=    AtomicValues            (T-Atom)
        fn name: PlainType. t   (T-Abs)
        t t                     (T-App)
        let id = t in t         (T-Let)
        if t then t else t      (T-If)
        loop t t                (T-Loop)
        [t, ..., t]             (T-Tuple)
        TyName[t, ..., t]       (T-NamedTuple)
        t.IntLiteral            (T-TupleAccess)
        TyName{li = t, ..., lj = t} (T-Record)
        t.l                     (T-RecordAccess)
        -t                      (T-Minus)
        t op t                  (T-BinaryOp)
    
AtomicValues := IntLiteral
                FloatLiteral
                BoolLiteral

v :=    unit
        AtomicValues
        TyName{li = v, ..., lj = v}
        [v, ..., v]
        TyName[v, ..., v]

T :=    PlainType
        T <'c>-> T

PlainType :=    TyName
                (PlainType, ..., PlainType)
                float
                int
                bool

TypeDeclare :=  type TyName(PlainType, ... , PlainType)
                type Tyname{li: PlainType, ..., lj: PlainType}

TopLevel := uniform name: PlainType
            extern func_name: T -> T
            TypDeclare
            let name = t
            entry name = t
```

`'c` in function type `T <'c>-> T` is auto generated function id, so each function has different type, which makes code gen simple as calling context type and method to call are all deterministic.

Users will not write any `'c`. If one writes `T1 -> T2` for type checking, he declares a template type with parameter `'c`, which will be instantiated later as `T1 <'c>-> T2`.

We currently do not allow function as argument to avoid complex generic types.

Entry functions are visible in the translated GLSL code. 

Types should be forward declared. We don't support recursive types.

## Typing

```

```

## Translation

```
```