// https://observablehq.com/@znah/particle-lenia-from-scratch

open System
open System.IO

open SkiaSharp

let parms =
    {|
        mu_k = 4.0
        sigma_k = 1.0
        w_k = 0.022
        mu_g = 0.6
        sigma_g = 0.15
        c_rep = 1.0
        dt = 0.2
    |}

let point_n = 400

let random = Random(0)

[<Struct>]
type Point =
    { X : float; Y : float }

    static member Zero = { X = 0; Y = 0 }

    static member inline (+)(a, b) =
        { X = a.X + b.X; Y = a.Y + b.Y }

    static member inline (-)(a, b) =
        { X = a.X - b.X; Y = a.Y - b.Y }

    static member inline (~-)(a) =
        { X = -a.X; Y = -a.Y }

    static member inline (*)(a, b) =
        { X = a * b.X; Y = a * b.Y }

    static member inline (/)(a, b) =
        { X = a.X / b; Y = a.Y / b }

let repulsion_f x c_rep =
    let t = max (1.0 - x) 0.0
    0.5*c_rep*t*t, -c_rep*t

let peak_f x mu sigma w =
    let t = (x-mu)/sigma
    let y = w / exp(t*t)
    y, -2.0*t*y/sigma

let compute_fields (points : _[]) =

    let c_rep = parms.c_rep
    let mu_k = parms.mu_k
    let sigma_k = parms.sigma_k
    let w_k = parms.w_k

    let upper =
        [|
            for i = 0 to point_n - 1 do
                [|
                    for j = i to point_n - 1 do
                        let diff = points[i] - points[j]
                        let r = sqrt(diff.X*diff.X + diff.Y*diff.Y) + 1e-20
                        let dr = diff / r  // unit length ∇r
  
                        // ∇R = R'(r) ∇r
                        let R, dR = repulsion_f r c_rep

                        // ∇K = K'(r) ∇r
                        let K, dK = peak_f r mu_k sigma_k w_k

                        struct {| R=R; dR=dR*dr; K=K; dK=dK*dr |}
                |]
        |]

    let lookup i j =
        if i <= j then upper[i][j-i]
        else
            let v = upper[j][i-j]
            struct {| v with dR = -v.dR; dK = -v.dK |}

    [|
        for i = 0 to point_n - 1 do
            let vs =
                [| for j = 0 to point_n - 1 do lookup i j |]
            {|
                R_grad = vs |> Array.sumBy _.dR
                R_val = vs |> Array.sumBy _.R
                U_grad = vs |> Array.sumBy _.dK
                U_val = vs |> Array.sumBy _.K
            |}
    |]

let step points =

    let mu_g = parms.mu_g
    let sigma_g = parms.sigma_g
    let dt = parms.dt

    let fields = compute_fields points
    [|
        for i = 0 to point_n - 1 do
            let G, dG = peak_f fields[i].U_val mu_g sigma_g 1.0
            // [vx, vy] = -∇E = G'(U)∇U - ∇R
            let vpt = dG*fields[i].U_grad - fields[i].R_grad
            yield points[i] + (dt * vpt)
    |], fields

let stepsPerFrame = 10
let world_width = 40.0

let mutable points =
    let coord () = (random.NextDouble() - 0.5) * 36.0
    Array.init point_n (fun _ ->
        { X = coord (); Y = coord () })

let animate (outputDir: string) frameIndex =
    let mutable fields = Array.empty
    for _ in 1 .. stepsPerFrame do
        let points_new, fields_new = step points
        points <- points_new
        fields <- fields_new

    let width, height = 1000, 1000 // Canvas size
    let surface = SKSurface.Create(SKImageInfo(width, height))
    let canvas = surface.Canvas

    // Reset and clear canvas
    canvas.Clear(SKColors.White)
    canvas.Translate(float32 width / 2.0f, float32 height / 2.0f)
    let scale = float width / world_width
    canvas.Scale(float32 scale, float32 scale)

    // Draw particles
    use paint = new SKPaint()
    paint.Color <- SKColors.Black
    paint.Style <- SKPaintStyle.Stroke
    paint.StrokeWidth <- 0.1f

    for i = 0 to point_n - 1 do
        let pt = points[i]
        let r = parms.c_rep / (fields[i].R_val * 5.0) // Calculate radius based on repulsion
        canvas.DrawCircle(float32 pt.X, float32 pt.Y, float32 r, paint)

    // Save the frame as a PNG file
    use image = surface.Snapshot()
    use data = image.Encode(SKEncodedImageFormat.Png, 100)
    let filePath = Path.Combine(outputDir, sprintf "frame_%04d.png" frameIndex)
    use stream = File.Create(filePath)
    data.SaveTo(stream)
    
    printfn "Saved %s" filePath

let outputDir = "Output"
Directory.CreateDirectory(outputDir) |> ignore
for frameIndex in 1 .. 1000 do
    animate outputDir frameIndex

// C:\users\brian\Downloads\ffmpeg-7.1.1-essentials_build\bin\ffmpeg.exe -framerate 30 -i "frame_%04d.png" -c:v libx264 -pix_fmt yuv420p output.mp4
