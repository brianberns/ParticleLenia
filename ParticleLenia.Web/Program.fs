namespace ParticleLenia.Web

open System

open Browser
open Browser.Types
open Fable.Core.JsInterop

module Program =

    let settings =
        {
            mu_k = 4.0
            sigma_k = 1.0
            w_k = 0.022
            mu_g = 0.6
            sigma_g = 0.15
            c_rep = 1.0
            dt = 0.1
        }

    let canvas_width, canvas_height = 800.0, 800.0
    let canvas =
        document.getElementById "canvas"
            :?> HTMLCanvasElement
    canvas.width <- canvas_width
    canvas.height <- canvas_height
    let canvas_width_half = canvas_width / 2.0
    let canvas_height_half = canvas_height / 2.0

    let ctx = canvas.getContext_2d()
    ctx.lineWidth <- 0.05

    let steps_per_frame = 10
    let world_width = 25.0

    let animate points =

        let points, fields =
            ((points, Array.empty), [1 .. steps_per_frame])
                ||> Seq.fold (fun (points, _) _ ->
                    Engine.step settings points)

        ctx.clearRect(0, 0, canvas_width, canvas_height)
        ctx.translate(canvas_width_half, canvas_height_half)
        let s = canvas_width / world_width
        ctx.scale(s, s)
        for i = 0 to points.Length - 1 do

            let field = fields[i]
            let color =
                let E = field.R_val - field.G
                assert(E >= -1.0)
                assert(E <= 1.0)
                let E_norm = (E / 1.5) + 0.5
                assert(E_norm >= 0.0)
                assert(E_norm <= 1.0)
                let hue = 360.0 * E_norm
                $"hsl({hue}, 100%%, 50%%)"

            ctx.beginPath()
            let pt = points[i]
            let r = settings.c_rep / (field.R_val * 5.0)
            ctx.arc(pt.X, pt.Y, r, 0.0, Math.PI * 2.0)
            ctx.fillStyle <-  !^color
            ctx.fill()
            ctx.stroke()

        ctx.setTransform(1.0, 0.0, 0.0, 1.0, 0.0, 0.0)   // resetTransform() not available in Fable?

        points

    let rec loop iFrame prev points =
        window.requestAnimationFrame(fun timestamp ->
            let cur =
                if iFrame % 100 = 0 then
                    console.log($"{(timestamp - prev) / 100.0} ms/step")
                    timestamp
                else prev
            animate points
                |> loop (iFrame + 1) cur)
                |> ignore

    let random = Random(0)

    let makePoints nPoints scale offset =
        Array.init nPoints (fun _ ->
            {
                X = (random.NextDouble()) * scale.X + offset.X
                Y = (random.NextDouble()) * scale.Y + offset.Y
            })

    let points =
        let n = 50
        let scale = 8.0
        let offset = 2.5
        [|
            yield! makePoints n
                (Point.create scale scale)
                (Point.create offset offset)
            yield! makePoints n
                (Point.create -scale scale)
                (Point.create -offset offset)
            yield! makePoints n
                (Point.create scale -scale)
                (Point.create offset -offset)
            yield! makePoints n
                (Point.create -scale -scale)
                (Point.create -offset -offset)
        |]

    loop 0 0.0 points
