import cairo as _cairo
from manim.camera.camera import Camera as _Camera

_state = {}
_KNOBS = {'tolerance': None, 'antialias': None, 'aa_max_alpha': None, 'skip_invisible': False}
_STATS = {'batches': 0, 'vmobjects': 0, 'skipped': 0, 'coarse': 0}


def _max_alpha(vm):
    # Largest alpha this VMobject paints with: fill, and strokes of nonzero width.
    m = 0.0
    for rgbas, width in (('fill_rgbas', 1.0), ('stroke_rgbas', getattr(vm, 'stroke_width', 0)),
                         ('background_stroke_rgbas', getattr(vm, 'background_stroke_width', 0))):
        try:
            w = float(width)
        except (TypeError, ValueError):
            return 1.0
        if w <= 0:
            continue
        a = getattr(vm, rgbas, None)
        if a is None:
            continue
        try:
            if len(a):
                m = max(m, float(a[:, 3].max()))
        except Exception:
            return 1.0
    return m


def _visible(vms):
    # Drop VMobjects that paint nothing (alpha 0 everywhere). Kept when the batch
    # holds an empty VMobject: Manim repaints the previous path for it.
    for vm in vms:
        if len(vm.points) == 0:
            return vms
    keep = [vm for vm in vms if _max_alpha(vm) > 0]
    _STATS['skipped'] += len(vms) - len(keep)
    return keep


def desargues_preview_aa_install(tolerance, antialias, aa_max_alpha, skip_invisible):
    _KNOBS.update(tolerance=tolerance, antialias=antialias, aa_max_alpha=aa_max_alpha,
                  skip_invisible=bool(skip_invisible))
    if '_desargues_preview_aa' in _Camera.__dict__:
        return True
    orig = _Camera.__dict__['display_multiple_non_background_colored_vmobjects']
    K, S = _KNOBS, _STATS

    def display_multiple_non_background_colored_vmobjects(self, vmobjects, pixel_array):
        inner = _state['inner']
        ctx = self.get_cairo_context(pixel_array)
        if type(ctx) is not _cairo.Context:
            return inner(self, vmobjects, pixel_array)
        # Manim hands an itertools group: materialize it once.
        vmobjects = list(vmobjects)
        S['batches'] += 1
        S['vmobjects'] += len(vmobjects)
        if K['skip_invisible']:
            vmobjects = _visible(vmobjects)
        ctx.save()
        try:
            if K['tolerance'] is not None:
                ctx.set_tolerance(K['tolerance'])
            aa, thr = K['antialias'], K['aa_max_alpha']
            if aa is None:
                inner(self, vmobjects, pixel_array)
            elif thr is None:
                ctx.set_antialias(_cairo.Antialias(aa))
                S['coarse'] += len(vmobjects)
                inner(self, vmobjects, pixel_array)
            else:
                # Coarse antialiasing only for faint VMobjects (max alpha <= thr),
                # in runs so the batch order is Manim's.
                default = ctx.get_antialias()
                run, flag = [], None
                for vm in vmobjects:
                    f = _max_alpha(vm) <= thr
                    if f != flag and run:
                        ctx.set_antialias(_cairo.Antialias(aa) if flag else default)
                        inner(self, run, pixel_array)
                        run = []
                    flag = f
                    run.append(vm)
                    if f:
                        S['coarse'] += 1
                if run:
                    ctx.set_antialias(_cairo.Antialias(aa) if flag else default)
                    inner(self, run, pixel_array)
        finally:
            ctx.restore()
        return None

    _state['inner'] = orig
    _Camera.display_multiple_non_background_colored_vmobjects = display_multiple_non_background_colored_vmobjects
    _Camera._desargues_preview_aa = True
    return True


def desargues_preview_aa_uninstall():
    if '_desargues_preview_aa' not in _Camera.__dict__:
        return False
    _Camera.display_multiple_non_background_colored_vmobjects = _state.pop('inner')
    del _Camera._desargues_preview_aa
    return True


def desargues_preview_aa_installed():
    return '_desargues_preview_aa' in _Camera.__dict__


def desargues_preview_aa_stats():
    return dict(_STATS)
