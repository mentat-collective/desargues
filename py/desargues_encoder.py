"""Encoder-profile shim for Manim CE's partial-movie writer.

Manim hard-codes libx264 / yuv420p / crf 23 (x264's default preset, medium)
in SceneFileWriter.open_partial_movie_stream. install(codec, pix_fmt, options)
replaces that method with one that opens the stream with the given codec,
pixel format and codec options. The final movie is a remux of the partial
files (combine_files copies packets), so the partial codec IS the output codec.

Transparent and .webm renders keep Manim's own method: their codec choice is
format-driven (qtrle/argb, libvpx-vp9) and a profile must not break them.

install() is idempotent: reinstalling replaces the profile, uninstall()
restores the original method.
"""

from queue import Queue
from threading import Thread

import av
from manim import config
from manim.scene.scene_file_writer import SceneFileWriter, to_av_frame_rate

_ORIGINAL = getattr(SceneFileWriter, "_desargues_original_open", None) \
    or SceneFileWriter.open_partial_movie_stream
SceneFileWriter._desargues_original_open = _ORIGINAL

_PROFILE = None


def current():
    """The installed profile as (codec, pix_fmt, options), or None."""
    return _PROFILE


def _profiled_open(self, file_path=None):
    if config.transparent or config.movie_file_extension == ".webm" or _PROFILE is None:
        return _ORIGINAL(self, file_path)

    codec, pix_fmt, options = _PROFILE
    if file_path is None:
        file_path = self.partial_movie_files[self.renderer.num_plays]
    self.partial_movie_file_path = file_path

    av_options = {"an": "1"}
    av_options.update(options)

    video_container = av.open(file_path, mode="w")
    stream = video_container.add_stream(
        codec, rate=to_av_frame_rate(config.frame_rate), options=av_options
    )
    stream.pix_fmt = pix_fmt
    stream.width = config.pixel_width
    stream.height = config.pixel_height

    self.video_container = video_container
    self.video_stream = stream
    self.queue = Queue()
    self.writer_thread = Thread(target=self.listen_and_write, args=())
    self.writer_thread.start()


def install(codec, pix_fmt, options):
    """Make every subsequent partial movie use codec/pix_fmt/options.

    options: mapping of ffmpeg codec option name -> value (stringified)."""
    global _PROFILE
    _PROFILE = (str(codec), str(pix_fmt), {str(k): str(v) for k, v in dict(options).items()})
    SceneFileWriter.open_partial_movie_stream = _profiled_open
    return _PROFILE


def uninstall():
    global _PROFILE
    _PROFILE = None
    SceneFileWriter.open_partial_movie_stream = _ORIGINAL
