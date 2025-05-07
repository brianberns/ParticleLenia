namespace ParticleLenia.Web

open Browser

module Program =
    let div = document.createElement "div"
    div.innerHTML <- "Hello world!"
    document.body.appendChild div |> ignore
