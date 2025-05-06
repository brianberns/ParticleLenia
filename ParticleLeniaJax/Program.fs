// https://colab.research.google.com/github/google-research/self-organising-systems/blob/master/notebooks/particle_lenia.ipynb

open System.Diagnostics
open DiffSharp

dsharp.config(device=Device.GPU, backend=Backend.Torch)

/// Computes the value of a Gaussian peak at the given point with
/// the given mean and standard deviation.
let peak_f (x : Tensor) (mu : float (*mean*)) (sigma : float (*std dev*)) =
    exp -(((x-mu)/sigma)**2)

let mu_k = 4.0
let sigma_k = 1.0
let w_k = 0.022
let mu_g = 0.6
let sigma_g = 0.15
let c_rep = 1.0

let square x = x * x

let fields_f (points : Tensor) (x : Tensor) =
    let x_expanded =   // expand x to match points
        let shape =
            Array.append
                x.shape[0 .. x.shape.Length - 2]
                points.shape
        x.unsqueeze(-2).expand(shape)
    let diff = square(x_expanded-points).sum(-1)
    let r = sqrt(diff.clamp(1e-10))
    let U = (peak_f r mu_k sigma_k).sum(-1)*w_k
    let G = peak_f U mu_g sigma_g
    let R = c_rep/2.0 * ((1.0-r).clamp(0.0)**2).sum(-1)
    {| diff=diff; U=U; G=G; R=R; E=R-G |}

let vmap f (inputs : Tensor) =
    Array.init inputs.shape[0] id
        |> Array.Parallel.map (fun i ->
            f inputs[i])
        |> dsharp.stack

let motion_f points =
    let grad_E = dsharp.grad (fun x -> (fields_f points x).E)
    -(vmap grad_E points)

let points0 =
    (dsharp.rand([200; 2]) - 0.5) * 12.0
let dt = 0.1

let odeint_euler f x0 dt n =
    let step_f x _ =
        x+dt*(f x)
    Seq.scan step_f x0 [1..n]

type VideoWriter(fps, filename) =

    let mutable ffmpeg = None

    member this.add(img : Tensor) =
        let h, w = img.shape[0], img.shape[1]
        if ffmpeg = None then
            let startInfo =
                ProcessStartInfo(
                    FileName = "/Users/brian/Downloads/ffmpeg-7.1.1-essentials_build/bin/ffmpeg",
                    Arguments = $"-y -f rawvideo -vcodec rawvideo -s {w}x{h} -pix_fmt rgb24 -r {fps} -i - -pix_fmt yuv420p -c:v libx264 -crf 20 {filename}",
                    RedirectStandardInput = true,
                    UseShellExecute = false,
                    CreateNoWindow = false)
            let proc = Process.Start(startInfo)
            ffmpeg <- Some proc.StandardInput.BaseStream
        let img = (img.clamp(0, 1) * 255).cast(Dtype.Byte)
        ffmpeg.Value.Write(img.flatten().toArray1D<byte>())

let mgrid (startX : float) (stopX : float) numX (startY : float) (stopY : float) numY =
    let x = dsharp.linspace(startX, stopX, numX)
    let y = dsharp.linspace(startY, stopY, numY)
    let xGrid = x.unsqueeze(0).expand([numY; numX])
    let yGrid = y.unsqueeze(1).expand([numY; numX])
    dsharp.stack([xGrid; yGrid], dim=2)

let cmap_e e =
    let clamped = dsharp.stack([e; -e], dim=2).clamp(0)
    let colorTransformMatrix = dsharp.tensor([[0.3; 1.0; 1.0]; [1.0; 0.3; 1.0]])
    1.0 - dsharp.matmul(clamped, colorTransformMatrix)

let show_lenia (points : Tensor) extent =
    let w = 400
    let xy = mgrid -1.0 1.0 w -1.0 1.0 w * extent
    let e0 = -peak_f (dsharp.tensor 0.0) mu_g sigma_g
    let f = fields_f points
    let fields = f xy
    let r2 = fields.diff.min(fields.diff.shape.Length - 1)   // index -1 not allowed
    let points_mask = (r2/0.02).clamp(0, 1.0).unsqueeze(-1)
    cmap_e (fields.E - e0) * points_mask

let animate_lenia tracks name =
    let vid = VideoWriter(60, name)
    let extent = dsharp.abs(tracks).max()*1.2
    let bar_len = tracks.shape[0]
    for i = 0 to bar_len - 1 do
        let points = tracks[i]
        if i % 10 = 0 then
            let img = show_lenia points extent
            vid.add(img)

let rotor_story =
    odeint_euler motion_f points0 dt 100
        |> Seq.skip 1
        |> dsharp.stack
animate_lenia rotor_story "rotor.mp4"
