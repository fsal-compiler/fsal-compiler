﻿[<RequireQualifiedAccess>]
module Fs.AL.Compiler.Fullname.Operators

// module to store all the allowed operators
let [<Literal>] op_PipeRight = "Microsoft.FSharp.Core.Operators.(|>)"
let [<Literal>] op_Add = "Microsoft.FSharp.Core.Operators.(+)"
let [<Literal>] op_Subtract = "Microsoft.FSharp.Core.Operators.(-)"

//

let [<Literal>] cos = "Microsoft.FSharp.Core.Operators.cos"
let [<Literal>] cosh = "Microsoft.FSharp.Core.Operators.cosh"
let [<Literal>] sin = "Microsoft.FSharp.Core.Operators.sin"
let [<Literal>] sinh = "Microsoft.FSharp.Core.Operators.sinh"
let [<Literal>] tan = "Microsoft.FSharp.Core.Operators.tan"
let [<Literal>] tanh = "Microsoft.FSharp.Core.Operators.tanh"
let [<Literal>] op_Exponentiation = "Microsoft.FSharp.Core.Operators.op_Exponentiation"
let [<Literal>] pown = "Microsoft.FSharp.Core.Operators.pown"
let [<Literal>] nativeint = "Microsoft.FSharp.Core.Operators.nativeint"
let [<Literal>] string = "Microsoft.FSharp.Core.Operators.string"
let [<Literal>] char = "Microsoft.FSharp.Core.Operators.char"
let [<Literal>] lock = "Microsoft.FSharp.Core.Operators.lock"
let [<Literal>] using = "Microsoft.FSharp.Core.Operators.using"
let [<Literal>] typeof = "Microsoft.FSharp.Core.Operators.typeof"
let [<Literal>] nameof = "Microsoft.FSharp.Core.Operators.nameof"
let [<Literal>] typedefof = "Microsoft.FSharp.Core.Operators.typedefof"
let [<Literal>] sizeof = "Microsoft.FSharp.Core.Operators.sizeof"
let [<Literal>] hash = "Microsoft.FSharp.Core.Operators.hash"
let [<Literal>] limitedHash = "Microsoft.FSharp.Core.Operators.limitedHash"
let [<Literal>] id = "Microsoft.FSharp.Core.Operators.id"
let [<Literal>] not = "Microsoft.FSharp.Core.Operators.``not``"
let [<Literal>] stdin = "Microsoft.FSharp.Core.Operators.stdin"
let [<Literal>] stdout = "Microsoft.FSharp.Core.Operators.stdout"
let [<Literal>] stderr = "Microsoft.FSharp.Core.Operators.stderr"
let [<Literal>] op_Range = "Microsoft.FSharp.Core.Operators.op_Range"
let [<Literal>] op_RangeStep = "Microsoft.FSharp.Core.Operators.op_RangeStep"
let [<Literal>] abs = "Microsoft.FSharp.Core.Operators.abs"
let [<Literal>] acos = "Microsoft.FSharp.Core.Operators.acos"
let [<Literal>] asin = "Microsoft.FSharp.Core.Operators.asin"
let [<Literal>] atan = "Microsoft.FSharp.Core.Operators.atan"
let [<Literal>] atan2 = "Microsoft.FSharp.Core.Operators.atan2"
let [<Literal>] ceil = "Microsoft.FSharp.Core.Operators.ceil"
let [<Literal>] exp = "Microsoft.FSharp.Core.Operators.exp"
let [<Literal>] floor = "Microsoft.FSharp.Core.Operators.floor"
let [<Literal>] truncate = "Microsoft.FSharp.Core.Operators.truncate"
let [<Literal>] round = "Microsoft.FSharp.Core.Operators.round"
let [<Literal>] sign = "Microsoft.FSharp.Core.Operators.sign"
let [<Literal>] log = "Microsoft.FSharp.Core.Operators.log"
let [<Literal>] log10 = "Microsoft.FSharp.Core.Operators.log10"
let [<Literal>] sqrt = "Microsoft.FSharp.Core.Operators.sqrt"
let [<Literal>] op_UnaryPlus = "Microsoft.FSharp.Core.Operators.op_UnaryPlus"
let [<Literal>] op_LeftShift = "Microsoft.FSharp.Core.Operators.op_LeftShift"
let [<Literal>] op_RightShift = "Microsoft.FSharp.Core.Operators.op_RightShift"
let [<Literal>] op_BitwiseAnd = "Microsoft.FSharp.Core.Operators.op_BitwiseAnd"
let [<Literal>] op_BitwiseOr = "Microsoft.FSharp.Core.Operators.op_BitwiseOr"
let [<Literal>] op_ExclusiveOr = "Microsoft.FSharp.Core.Operators.op_ExclusiveOr"
let [<Literal>] op_LogicalNot = "Microsoft.FSharp.Core.Operators.op_LogicalNot"
let [<Literal>] op_Append = "Microsoft.FSharp.Core.Operators.op_Append"
let [<Literal>] incr = "Microsoft.FSharp.Core.Operators.incr"
let [<Literal>] decr = "Microsoft.FSharp.Core.Operators.decr"
let [<Literal>] exit = "Microsoft.FSharp.Core.Operators.exit"
let [<Literal>] byte = "Microsoft.FSharp.Core.Operators.byte"
let [<Literal>] sbyte = "Microsoft.FSharp.Core.Operators.sbyte"
let [<Literal>] uint16 = "Microsoft.FSharp.Core.Operators.uint16"
let [<Literal>] int16 = "Microsoft.FSharp.Core.Operators.int16"
let [<Literal>] uint32 = "Microsoft.FSharp.Core.Operators.uint32"
let [<Literal>] int32 = "Microsoft.FSharp.Core.Operators.int32"
let [<Literal>] int = "Microsoft.FSharp.Core.Operators.int"
let [<Literal>] uint = "Microsoft.FSharp.Core.Operators.uint"
let [<Literal>] enum = "Microsoft.FSharp.Core.Operators.enum"
//    let [<Literal>] |KeyValue| = "Microsoft.FSharp.Core.Operators.|KeyValue|"
let [<Literal>] uint64 = "Microsoft.FSharp.Core.Operators.uint64"
let [<Literal>] int64 = "Microsoft.FSharp.Core.Operators.int64"
let [<Literal>] float32 = "Microsoft.FSharp.Core.Operators.float32"
let [<Literal>] float = "Microsoft.FSharp.Core.Operators.float"
let [<Literal>] decimal = "Microsoft.FSharp.Core.Operators.decimal"
let [<Literal>] unativeint = "Microsoft.FSharp.Core.Operators.unativeint"
let [<Literal>] seq = "Microsoft.FSharp.Core.Operators.seq"
let [<Literal>] unbox = "Microsoft.FSharp.Core.Operators.unbox"
let [<Literal>] box = "Microsoft.FSharp.Core.Operators.box"
let [<Literal>] tryUnbox = "Microsoft.FSharp.Core.Operators.tryUnbox"
let [<Literal>] isNull = "Microsoft.FSharp.Core.Operators.isNull"
let [<Literal>] raise = "Microsoft.FSharp.Core.Operators.raise"
let [<Literal>] Failure = "Microsoft.FSharp.Core.Operators.Failure"
//    let [<Literal>] |Failure|_| = "Microsoft.FSharp.Core.Operators.|Failure|_|"
let [<Literal>] op_LessThan = "Microsoft.FSharp.Core.Operators.(<)"
let [<Literal>] op_GreaterThan = "Microsoft.FSharp.Core.Operators.(>)"
let [<Literal>] op_GreaterThanOrEqual = "Microsoft.FSharp.Core.Operators.op_GreaterThanOrEqual" 
let [<Literal>] op_LessThanOrEqual = "Microsoft.FSharp.Core.Operators.op_LessThanOrEqual"       
let [<Literal>] op_Equality = "Microsoft.FSharp.Core.Operators.op_Equality"
let [<Literal>] op_NotEquals = "Microsoft.FSharp.Core.Operators.(<>)"
let [<Literal>] ``op_Equals`` = "Microsoft.FSharp.Core.Operators.(=)"
let [<Literal>] compare = "Microsoft.FSharp.Core.Operators.compare"
let [<Literal>] max = "Microsoft.FSharp.Core.Operators.max"
let [<Literal>] min = "Microsoft.FSharp.Core.Operators.min"

let [<Literal>] failwith = "Microsoft.FSharp.Core.Operators.failwith"
let [<Literal>] invalidArg = "Microsoft.FSharp.Core.Operators.invalidArg"
let [<Literal>] nullArg = "Microsoft.FSharp.Core.Operators.nullArg"
let [<Literal>] invalidOp = "Microsoft.FSharp.Core.Operators.invalidOp"
let [<Literal>] rethrow = "Microsoft.FSharp.Core.Operators.rethrow"
let [<Literal>] reraise = "Microsoft.FSharp.Core.Operators.reraise"
let [<Literal>] fst = "Microsoft.FSharp.Core.Operators.fst"
let [<Literal>] snd = "Microsoft.FSharp.Core.Operators.snd"
let [<Literal>] ignore = "Microsoft.FSharp.Core.Operators.ignore"
let [<Literal>] ref = "Microsoft.FSharp.Core.Operators.ref"
let [<Literal>] op_ColonEquals = "Microsoft.FSharp.Core.Operators.op_ColonEquals"
let [<Literal>] op_Dereference = "Microsoft.FSharp.Core.Operators.op_Dereference"

let [<Literal>] op_PipeLeft = "Microsoft.FSharp.Core.Operators.op_PipeLeft"
let [<Literal>] op_PipeLeft2 = "Microsoft.FSharp.Core.Operators.op_PipeLeft2"
let [<Literal>] op_PipeLeft3 = "Microsoft.FSharp.Core.Operators.op_PipeLeft3"
let [<Literal>] op_ComposeRight = "Microsoft.FSharp.Core.Operators.op_ComposeRight"
let [<Literal>] op_ComposeLeft = "Microsoft.FSharp.Core.Operators.op_ComposeLeft"
let [<Literal>] op_Concatenate = "Microsoft.FSharp.Core.Operators.op_Concatenate"
let [<Literal>] defaultArg = "Microsoft.FSharp.Core.Operators.defaultArg"
let [<Literal>] defaultValueArg = "Microsoft.FSharp.Core.Operators.defaultValueArg"
let [<Literal>] op_UnaryNegation = "Microsoft.FSharp.Core.Operators.op_UnaryNegation"
let [<Literal>] op_Addition = "Microsoft.FSharp.Core.Operators.op_Addition"
let [<Literal>] op_Subtraction = "Microsoft.FSharp.Core.Operators.op_Subtraction"
let [<Literal>] op_Multiply = "Microsoft.FSharp.Core.Operators.op_Multiply"
let [<Literal>] op_Division = "Microsoft.FSharp.Core.Operators.op_Division"
let [<Literal>] op_Modulus = "Microsoft.FSharp.Core.Operators.op_Modulus"
let [<Literal>] Infinity = "Microsoft.FSharp.Core.Operators.Infinity"
let [<Literal>] NaN = "Microsoft.FSharp.Core.Operators.NaN"
let [<Literal>] InfinitySingle = "Microsoft.FSharp.Core.Operators.InfinitySingle"
let [<Literal>] NaNSingle = "Microsoft.FSharp.Core.Operators.NaNSingle"

