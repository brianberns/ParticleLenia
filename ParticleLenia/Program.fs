open DiffSharp

let mu_k = 4.0
let sigma_k = 1.0
let w_k = 0.022
let mu_g = 0.6
let sigma_g = 0.15
let c_rep = 1.0

/// Computes the value of a Gaussian peak at the given point with
/// the given mean and standard deviation.
let peak_f (x : Tensor) (mu : Tensor (*mean*)) (sigma : Tensor (*std dev*)) =
    exp(-((x - mu) / sigma) ** 2)

let square x = x * x

let fields_f (points : Tensor) (x : Tensor) =
  let r =
    sqrt(
        square(x - points)
            .sum(-1)
            .clamp(1e-10))
  (*
  U = peak_f(r, p.mu_k, p.sigma_k).sum()*p.w_k
  G = peak_f(U, p.mu_g, p.sigma_g)
  R = p.c_rep/2 * ((1.0-r).clip(0.0)**2).sum()
  return Fields(U, G, R, E=R-G)
  *)
  ()

let points0 =
    (dsharp.rand([200; 2]) - 0.5) * 12.0
printfn "%A" points0
let dt = 0.1
