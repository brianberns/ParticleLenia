import { Record } from "./fable_modules/fable-library-js.4.25.0/Types.js";
import { record_type, float64_type } from "./fable_modules/fable-library-js.4.25.0/Reflection.js";

export class Point extends Record {
    constructor(X, Y) {
        super();
        this.X = X;
        this.Y = Y;
    }
    toString() {
        const this$ = this;
        return `(${this$.X}, ${this$.Y})`;
    }
}

export function Point_$reflection() {
    return record_type("ParticleLenia.Web.Point", [], Point, () => [["X", float64_type], ["Y", float64_type]]);
}

/**
 * Origin.
 */
export function Point_get_Zero() {
    return new Point(0, 0);
}

/**
 * Adds two points component-wise.
 */
export function Point_op_Addition_16A3C40(p1, p2) {
    return new Point(p1.X + p2.X, p1.Y + p2.Y);
}

/**
 * Subtracts one point from another component-wise.
 */
export function Point_op_Subtraction_16A3C40(p1, p2) {
    return new Point(p1.X - p2.X, p1.Y - p2.Y);
}

/**
 * Multiplies a point by a scalar.
 */
export function Point_op_Multiply_5F589459(p, a) {
    return new Point(p.X * a, p.Y * a);
}

/**
 * Multiplies a scalar by a point.
 */
export function Point_op_Multiply_253241B9(a, p) {
    return new Point(a * p.X, a * p.Y);
}

/**
 * Divides a point by a scalar.
 */
export function Point_op_Division_5F589459(p, a) {
    return new Point(p.X / a, p.Y / a);
}

/**
 * Computes the dot product of two points (treated
 * as vectors).
 */
export function Point__Dot_AAF22(this$, other) {
    return (this$.X * other.X) + (this$.Y * other.Y);
}

/**
 * Computes the length of the point when considered
 * as a vector.
 */
export function Point__get_Length(this$) {
    return Math.sqrt((this$.X * this$.X) + (this$.Y * this$.Y));
}

