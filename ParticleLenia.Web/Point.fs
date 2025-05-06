namespace ParticleLenia.Web

/// 2D point.
type Point =
    { X : float; Y : float }

    /// Origin.
    static member Zero = { X = 0; Y = 0 }

    /// Negates a point.
    static member inline (~-)(a) =
        { X = -a.X; Y = -a.Y }
    
    /// Adds two points component-wise.
    static member (+)(p1, p2) =
        { X = p1.X + p2.X; Y = p1.Y + p2.Y }
    
    /// Subtracts one point from another component-wise.
    static member (-)(p1, p2) =
        { X = p1.X - p2.X; Y = p1.Y - p2.Y }
    
    /// Multiplies a point by a scalar.
    static member (*)(p, a) =
        { X = p.X * a; Y = p.Y * a }
    
    /// Multiplies a scalar by a point.
    static member (*)(a, p) =
        { X = a * p.X; Y = a * p.Y }
    
    /// Divides a point by a scalar.
    static member (/)(p, a) =
        { X = p.X / a; Y = p.Y / a }

    /// Computes the dot product of two points (treated
    /// as vectors).
    member this.Dot(other) =
        this.X * other.X + this.Y * other.Y

    /// Computes the length of the point when considered
    /// as a vector.
    member this.Length =
        sqrt (this.X * this.X + this.Y * this.Y)

    /// Pretty print.
    override this.ToString() =
        $"({this.X}, {this.Y})"

module Point =

    let create x y =
        { X = x; Y = y }
