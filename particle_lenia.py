# Imports & utils

import io
import base64
import time
from functools import partial
from typing import NamedTuple
from collections import namedtuple
import subprocess

import PIL
import numpy as np
import matplotlib.pylab as pl

from IPython.display import display, Image, HTML, clear_output
import ipywidgets as widgets

import jax
import jax.numpy as jp


def np2pil(a):
  if a.dtype in [np.float32, np.float64]:
    a = np.uint8(np.clip(a, 0, 1)*255)
  return PIL.Image.fromarray(a)

def imwrite(f, a, fmt=None):
  a = np.asarray(a)
  if isinstance(f, str):
    fmt = f.rsplit('.', 1)[-1].lower()
    if fmt == 'jpg':
      fmt = 'jpeg'
    f = open(f, 'wb')
  np2pil(a).save(f, fmt, quality=95)

def imencode(a, fmt='jpeg'):
  a = np.asarray(a)
  if len(a.shape) == 3 and a.shape[-1] == 4:
    fmt = 'png'
  f = io.BytesIO()
  imwrite(f, a, fmt)
  return f.getvalue()

def imshow(a, fmt='jpeg', display=display):
  return display(Image(data=imencode(a, fmt)))

def grab_plot(close=True):
  """Return the current Matplotlib figure as an image"""
  fig = pl.gcf()
  fig.canvas.draw()
  img = np.array(fig.canvas.renderer._renderer)
  a = np.float32(img[..., 3:]/255.0)
  img = np.uint8(255*(1.0-a) + img[...,:3] * a)  # alpha
  if close:
    pl.close()
  return img

def show_videofile(fn):
  b64 = base64.b64encode(open(fn, 'rb').read()).decode('utf8')
  s = f'''<video controls loop>
 <source src="data:video/mp4;base64,{b64}" type="video/mp4">
 Your browser does not support the video tag.</video>'''
  display(HTML(s))

class VideoWriter:
  def __init__(self, filename='_tmp.mp4', fps=30.0, show_on_finish=True):
    self.ffmpeg = None
    self.filename = filename
    self.fps = fps
    self.view = widgets.Output()
    self.last_preview_time = 0.0
    self.frame_count = 0
    self.show_on_finish = show_on_finish
    display(self.view)

  def add(self, img):
    img = np.asarray(img)
    h, w = img.shape[:2]
    if self.ffmpeg is None:
      self.ffmpeg = self._open(w, h)
    if img.dtype in [np.float32, np.float64]:
      img = np.uint8(img.clip(0, 1)*255)
    if len(img.shape) == 2:
      img = np.repeat(img[..., None], 3, -1)
    self.ffmpeg.stdin.write(img.tobytes())
    t = time.time()
    self.frame_count += 1
    if self.view and t-self.last_preview_time > 1.0:
       self.last_preview_time = t
       with self.view:
         clear_output(wait=True)
         imshow(img)
         print(self.frame_count)

  def __call__(self, img):
    return self.add(img)

  def _open(self, w, h):
    cmd = f'''/Users/brian/Downloads/ffmpeg-7.1.1-essentials_build/bin/ffmpeg -y -f rawvideo -vcodec rawvideo -s {w}x{h}
      -pix_fmt rgb24 -r {self.fps} -i - -pix_fmt yuv420p
      -c:v libx264 -crf 20 {self.filename}'''.split()
    return subprocess.Popen(cmd, stdin=subprocess.PIPE, stderr=subprocess.PIPE)

  def close(self):
    if self.ffmpeg:
        self.ffmpeg.stdin.close()
        self.ffmpeg.wait()
        self.ffmpeg = None
    if self.view:
      with self.view:
        clear_output()
      self.view.close()
      self.view = None

  def __enter__(self):
    return self

  def __exit__(self, *kw):
    self.close()
    if self.show_on_finish:
        self.show()

  def _ipython_display_(self):
    self.show()

  def show(self):
      self.close()
      show_videofile(self.filename)

# JAX utils

def vmap2(f):
  return jax.vmap(jax.vmap(f))

def norm(v, axis=-1, keepdims=False, eps=0.0):
  return jp.sqrt((v*v).sum(axis, keepdims=keepdims).clip(eps))

def normalize(v, axis=-1, eps=1e-20):
  return v/norm(v, axis, keepdims=True, eps=eps)

pl.rcParams.update({"axes.grid" : True})

# Particles and fields

Params = namedtuple('Params', 'mu_k sigma_k w_k mu_g sigma_g c_rep')
Fields = namedtuple('Fields', 'U G R E')

def peak_f(x, mu, sigma):
  return jp.exp(-((x-mu)/sigma)**2)

def fields_f(p: Params, points, x):
  r = jp.sqrt(jp.square(x-points).sum(-1).clip(1e-10))
  U = peak_f(r, p.mu_k, p.sigma_k).sum()*p.w_k
  G = peak_f(U, p.mu_g, p.sigma_g)
  R = p.c_rep/2 * ((1.0-r).clip(0.0)**2).sum()
  return Fields(U, G, R, E=R-G)

def motion_f(params, points):
  grad_E = jax.grad(lambda x : fields_f(params, points, x).E)
  return -jax.vmap(grad_E)(points)

# show lenia

import PIL.ImageFont, PIL.ImageDraw

def lerp(x, a, b):
  return jp.float32(a)*(1.0-x) + jp.float32(b)*x
def cmap_e(e):
  return 1.0-jp.stack([e, -e], -1).clip(0) @ jp.float32([[0.3,1,1], [1,0.3,1]])
def cmap_ug(u, g):
  vis = lerp(u[...,None], [0.1,0.1,0.3], [0.2,0.7,1.0])
  return lerp(g[...,None], vis, [1.17,0.91,0.13])

@partial(jax.jit, static_argnames=['w', 'show_UG', 'show_cmap'])
def show_lenia(params, points, extent, w=400, show_UG=False, show_cmap=True):
  xy = jp.mgrid[-1:1:w*1j, -1:1:w*1j].T*extent
  e0 = -peak_f(0.0, params.mu_g, params.sigma_g)
  f = partial(fields_f, params, points)
  fields = vmap2(f)(xy)
  r2 = jp.square(xy[...,None,:]-points).sum(-1).min(-1)
  points_mask = (r2/0.02).clip(0, 1.0)[...,None]
  vis = cmap_e(fields.E-e0) * points_mask
  if show_cmap:
    e_mean = jax.vmap(f)(points).E.mean()
    bar = np.r_[0.5:-0.5:w*1j]
    bar = cmap_e(bar) * (1.0-peak_f(bar, e_mean-e0, 0.005)[:,None])
    vis = jp.hstack([vis, bar[:,None].repeat(16, 1)])
  if show_UG:
    vis_u = cmap_ug(fields.U, fields.G)*points_mask
    if show_cmap:
      u = np.r_[1:0:w*1j]
      bar = cmap_ug(u, peak_f(u, params.mu_g, params.sigma_g))
      bar = bar[:,None].repeat(16, 1)
      vis_u = jp.hstack([bar, vis_u])
    vis = jp.hstack([vis_u, vis])
  return vis

fontpath = pl.matplotlib.get_data_path()+'/fonts/ttf/DejaVuSansMono.ttf'
pil_font = PIL.ImageFont.truetype(fontpath, size=16)

def text_overlay(img, text, pos=(20,10), color=(255,255,255)):
  img = np2pil(img)
  draw = PIL.ImageDraw.Draw(img)
  draw.text(pos, text, fill=color, font=pil_font)
  return img

def animate_lenia(params, tracks, rate=10, slow_start=0, w=400, show_UG=True,
                  name='_tmp.mp4', text=None, vid=None, bar_len=None,
                  bar_ofs=0, extent=None):
  if vid is None:
    vid = VideoWriter(fps=60, filename=name)
  if extent is None:
    extent = jp.abs(tracks).max()*1.2
  if bar_len is None:
    bar_len = len(tracks)
  for i, points in enumerate(tracks):
    if not (i<slow_start or i%rate==0):
      continue
    img = show_lenia(params, points, extent, w=w, show_UG=show_UG)
    bar = np.linspace(0, bar_len, img.shape[1])
    bar = (0.5+(bar>=i+bar_ofs)[:,None]*jp.ones(3)*0.5)[None].repeat(2, 0)
    frame = jp.vstack([img, bar])
    if text is not None:
      frame = text_overlay(frame, text)
    vid(frame)
  return vid

params = Params(mu_k=4.0, sigma_k=1.0, w_k=0.022, mu_g=0.6, sigma_g=0.15, c_rep=1.0)
key = jax.random.PRNGKey(20)
points0 = (jax.random.uniform(key, [200, 2])-0.5)*12.0
dt = 0.1

def odeint_euler(f, params, x0, dt, n):
  def step_f(x, _):
    x = x+dt*f(params, x)
    return x, x
  return jax.lax.scan(step_f, x0, None, n)[1]

rotor_story = odeint_euler(motion_f, params, points0, dt, 10000)
animate_lenia(params, rotor_story, name='rotor.mp4')
