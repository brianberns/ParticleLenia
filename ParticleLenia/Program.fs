open System

open MathNet.Numerics.LinearAlgebra
open MathNet.Numerics.Distributions

let parms =
    {|
        mu_k = 4.0
        sigma_k = 1.0
        w_k = 0.022
        mu_g = 0.6
        sigma_g = 0.15
        c_rep = 1.0
    |}

let points0 : Matrix<float> =
    DenseMatrix.random 200 2 (ContinuousUniform(-6., 6.))
printfn "%A" points0
