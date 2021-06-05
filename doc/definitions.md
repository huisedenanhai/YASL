# Definitions

## Syntax

```
t :=    AtomicValues            (T-Atom)
        fn name: ArgType. t     (T-Abs)
        t t                     (T-App)
        let id = t in t         (T-Let)
        let id: T = t in t      (T-LetCheck)
        if t then t else t      (T-If)
        loop t t                (T-Loop)
        (t, ..., t)             (T-Tuple)
        TyName(t, ..., t)       (T-NamedTuple)
        t.IntLiteral            (T-TupleAccess)
        TyName{li = t, ..., lj = t} (T-Record)
        t.l                     (T-RecordAccess)
        unit                    (T-Unit)
        -t                      (T-Minus)
        t op t                  (T-BinaryOp)
    
AtomicValues := IntLiteral
                FloatLiteral
                BoolLiteral

v :=    unit
        AtomicValues
        TyName{li = v, ..., lj = v}
        (v, ..., v)
        TyName(v, ..., v)

T :=    PlainType
        Unit
        T <'c>-> T

PlainType :=    TyName
                (PlainType, ..., PlainType)
                float
                int
                bool

ArgType :=      Unit
                PlainType

TypeDeclare :=  type TyName(PlainType, ... , PlainType)
                type Tyname{li: PlainType, ..., lj: PlainType}

TopLevel := uniform name: PlainType
            extern func_name: T -> T
            TypDeclare
            let name = t
            let name: T = t
            entry name = t
            entry name: T = t
```

`'c` in function type `T <'c>-> T` is used to distinguish typing context. Every well typed function has determinstic calling context, which makes code generation easier.

Users will not write any `'c`. If one writes `T1 -> T2 -> T3` for type checking, he declares a template type with parameter `'c`, which will be instantiated later as `T1 <'c>-> T2 <'c, T1>-> T3`.

We currently do not allow function as argument to avoid complex generic types.

Entry functions are visible in the translated GLSL code. They must be naive funtions.

Types should be forward declared. We don't support recursive type.

### Naive functions

A function type `T1 -> T2 -> .. -> Tn` is naive if
+ It is instantiated in the empty context
+ all `Ti`s are plain types or the type is `Unit -> PlainType`

## Typing

```

```

## Translation

```
```