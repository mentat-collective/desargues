"""Interleaved A/B of encoder profiles on one Manim scene.

Each render runs in a FRESH python process (no warm caches crossing arms
except the on-disk Tex cache, which the warm-up primes for every arm), and
reports wall time, CPU seconds (user+sys of self+children via getrusage),
output size, and the codec/preset ffprobe reads back from the output file.

usage: python bench/encoder_ab.py ROUNDS PROFILE... [--scene M:Class] [--quality q]
profiles are defined in PROFILES below and mirror desargues.manim.encoder.
"""
import json
import os
import subprocess
import sys
import tempfile

HERE = os.path.dirname(os.path.abspath(__file__))
ROOT = os.path.dirname(HERE)

# Mirrors (encoder-profile k) in desargues.manim.encoder; "final" is Manim's own.
PROFILES = {
    "final":   ["libx264", "yuv420p", {"crf": "23", "preset": "medium"}],
    "preview": ["libx264", "yuv420p", {"crf": "23", "preset": "ultrafast", "tune": "zerolatency"}],
    "preview-nozl": ["libx264", "yuv420p", {"crf": "23", "preset": "ultrafast"}],
    "superfast": ["libx264", "yuv420p", {"crf": "23", "preset": "superfast"}],
    "veryfast-t1": ["libx264", "yuv420p", {"crf": "23", "preset": "veryfast", "threads": "1"}],
    "mjpeg":   ["mjpeg", "yuvj420p", {"q:v": "5"}],
}

CHILD = r'''
import json, os, resource, sys, time
sys.path.insert(0, os.path.join(ROOT, "py"))
codec, pix_fmt, opts = PROFILE
t0 = time.perf_counter()
import desargues_encoder
from manim import config, tempconfig
import importlib
mod, cls = SCENE.split(":")
klass = getattr(importlib.import_module(mod), cls)
desargues_encoder.install(codec, pix_fmt, opts)
t1 = time.perf_counter()
with tempconfig({"quality": QUALITY, "media_dir": MEDIA, "disable_caching": True,
                 "verbosity": "ERROR", "progress_bar": "none",
                 "movie_file_extension": EXT}):
    s = klass(); s.render()
    out = str(s.renderer.file_writer.movie_file_path)
t2 = time.perf_counter()
ru = resource.getrusage(resource.RUSAGE_SELF); rc = resource.getrusage(resource.RUSAGE_CHILDREN)
print("RESULT" + json.dumps({"render_s": t2 - t1, "import_s": t1 - t0,
      "cpu_s": ru.ru_utime + ru.ru_stime + rc.ru_utime + rc.ru_stime,
      "out": out, "bytes": os.path.getsize(out)}))
'''


def probe(path):
    r = subprocess.run(["ffprobe", "-v", "error", "-select_streams", "v:0",
                        "-show_entries", "stream=codec_name,pix_fmt:format_tags=encoder",
                        "-of", "json", path], capture_output=True, text=True)
    info = json.loads(r.stdout)["streams"][0]
    # x264 writes its option string into the SEI; read the preset fingerprint back.
    sei = subprocess.run(["strings", "-n", "20", path], capture_output=True, text=True).stdout
    line = next((l for l in sei.splitlines() if "x264 - core" in l), "")
    for key in ("subme=", "me=", "ref=", "crf="):
        i = line.find(" " + key)
        if i >= 0:
            info[key.rstrip("=")] = line[i + 1:].split(" ")[0].split("=")[1]
    return info


def run(profile, scene, quality, media):
    ext = ".mov" if PROFILES[profile][0] == "mjpeg" else ".mp4"
    code = (f"ROOT={ROOT!r}\nPROFILE={PROFILES[profile]!r}\nSCENE={scene!r}\n"
            f"QUALITY={quality!r}\nMEDIA={media!r}\nEXT={ext!r}\n") + CHILD
    import time
    t0 = time.perf_counter()
    r = subprocess.run([sys.executable, "-c", code], capture_output=True, text=True, cwd=ROOT)
    wall = time.perf_counter() - t0
    line = next((l for l in r.stdout.splitlines() if l.startswith("RESULT")), None)
    if line is None:
        raise SystemExit(f"{profile} failed:\n{r.stderr[-3000:]}")
    d = json.loads(line[6:])
    d.update(profile=profile, proc_wall_s=wall, probe=probe(d["out"]))
    return d


def main():
    args = sys.argv[1:]
    scene, quality = "brachistochrone_full_derivation:BrachistochroneQuickDemo", "medium_quality"
    if "--scene" in args:
        i = args.index("--scene"); scene = args[i + 1]; del args[i:i + 2]
    if "--quality" in args:
        i = args.index("--quality"); quality = args[i + 1]; del args[i:i + 2]
    rounds, profiles = int(args[0]), args[1:]
    media = tempfile.mkdtemp(prefix="enc-ab-")
    for p in profiles:  # warm-up: tex cache, page cache, pyc
        run(p, scene, quality, media)
    rows = []
    for i in range(rounds):
        order = profiles if i % 2 == 0 else list(reversed(profiles))
        for p in order:
            d = run(p, scene, quality, media); d["round"] = i
            rows.append(d)
            print(json.dumps({k: d[k] for k in ("round", "profile", "render_s", "cpu_s", "bytes")}),
                  flush=True)
    print("SUMMARY")
    import statistics as st
    base = None
    for p in profiles:
        rs = [r for r in rows if r["profile"] == p]
        s = {"profile": p, "n": len(rs),
             "render_s_med": st.median(r["render_s"] for r in rs),
             "cpu_s_med": st.median(r["cpu_s"] for r in rs),
             "bytes": st.median(r["bytes"] for r in rs),
             "probe": rs[-1]["probe"]}
        if base is None:
            base = s
        s["wall_ratio"] = s["render_s_med"] / base["render_s_med"]
        s["cpu_ratio"] = s["cpu_s_med"] / base["cpu_s_med"]
        s["size_ratio"] = s["bytes"] / base["bytes"]
        print(json.dumps(s))


if __name__ == "__main__":
    main()
