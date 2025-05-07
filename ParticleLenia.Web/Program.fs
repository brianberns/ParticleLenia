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

    let canvas =
        document.getElementById "canvas"
            :?> HTMLCanvasElement
    canvas.width <- 800.0
    canvas.height <- 800.0

    let ctx = canvas.getContext_2d()

    let steps_per_frame = 10
    let world_width = 25.0

    let animate points =

        let points, fields =
            ((points, Array.empty), [1 .. steps_per_frame])
                ||> Seq.fold (fun (points, _) _ ->
                    Engine.step settings points)

        let width = canvas.width
        let height = canvas.height
        ctx.clearRect(0, 0, width, height)
        ctx.translate(width / 2.0, height / 2.0)
        let s = width / world_width
        ctx.scale(s, s)
        ctx.lineWidth <- 0.1
        for i = 0 to points.Length - 1 do
            ctx.beginPath()
            let pt = points[i]
            let r = settings.c_rep / (fields[i].R_val * 5.0)
            ctx.arc(pt.X, pt.Y, r, 0.0, Math.PI * 2.0)
            ctx.stroke()
        ctx.setTransform(1.0, 0.0, 0.0, 1.0, 0.0, 0.0)   // resetTransform() not available in Fable?

        points

    let rec loop points =
        window.requestAnimationFrame(fun _timestamp ->
            animate points
                |> loop)
                |> ignore

    let points =
        let nPoints = 200
        let random = Random(0)
        let coord () = (random.NextDouble() - 0.5) * 12.0
        Array.init nPoints (fun _ ->
            { X = coord (); Y = coord () })

    loop points
