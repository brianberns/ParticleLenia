namespace ParticleLenia.Web

type DualNumber =
    { Primal : float; Tangent : float }

module DualNumber =

    let create primal tangent =
        {
            Primal = primal
            Tangent = tangent
        }
