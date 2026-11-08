# Emmy upstream candidates

Things the step solver works around in desargues that belong in Emmy itself.
Each one has a local workaround, named here, that can be removed once Emmy
handles the case.

## `->TeX`: unknown operators come out sans-serif

Emmy's `->TeX` sets any operator it has no TeX rule for as `\mathsf{name}`.
Three cases appear in worked integrals:

| Emmy form   | Emmy's TeX             | Textbook TeX                 |
|-------------|------------------------|------------------------------|
| `(log u)`   | `\mathsf{log}(u)`      | `\ln u` (see below)          |
| `(ln u)`    | `\mathsf{ln}(u)`       | `\ln u`                      |
| `(exp u)`   | `\mathsf{exp}(u)`      | `e^{u}`, or `\exp u` for a long argument |
| `(abs u)`   | `\mathsf{abs}(u)`      | `\left\lvert u \right\rvert`, or `\left| u \right|` |

Textbook TeX uses the upright operator macros `\ln`, `\log` and `\exp`
(these also get the right spacing), and writes `exp` as a power of e when
the exponent is short.

**Naming: `log` means the natural log.** Emmy's `log` is the natural
logarithm. To many students (and in most calculators and school texts) a
bare log is base 10, so a worked solution that prints log for ln misleads
the reader. Pedro flagged this while reading the solutions. The fix is
in the renderer: `(log u)` should render as `\ln u`. Base-10 and base-b logs
(`log10`, `log2`) keep their subscripts.

**Workaround in desargues** (`desargues.steps.present`):

- Promote stage (frozen forms): `exp->e` rewrites `(exp u)` to
  `(expt e u)`, and `log->ln` rewrites `(log u)` to `(ln u)`.
- TeX fixes (strings): `\mathsf{ln}` becomes `\ln`, `\mathsf{abs}(u)`
  becomes `\left|u\right|` (`abs-bars`), and `\ln\left(\left|u\right|\right)`
  becomes `\ln\left|u\right|` (`ln-of-bars`).

**Upstream proposal:** give `->TeX` rules for `log` → `\ln`, `exp` →
`e^{u}` (with `\exp` past a length threshold), and `abs` → `\left|u\right|`,
and drop the parentheses after `\ln` when the argument is already bracketed
by bars.

## Autodiff: `abs` has no symbolic derivative

`emmy.autodiff`'s `g/abs` takes the sign of the primal (`(< f 0)`), so
`((D (fn [x] (abs x))) 'x)` throws a ClassCastException on a symbol. Any
antiderivative with `ln|u|` therefore has no symbolic derivative. Numeric
points still work, and they pick the branch.

**Workaround in desargues:** `desargues.steps.verify/antiderivative`
reports `:residual :undecided` when the symbolic derivative throws, and
bases its verdict on the numeric check. For log answers, choose check points
on every interval of the domain.

**Upstream proposal:** for a non-numeric primal, have `D` return
`(sgn u)·u'` (or `u/|u|·u'`), matching what `simplify` already knows about
`abs`.
