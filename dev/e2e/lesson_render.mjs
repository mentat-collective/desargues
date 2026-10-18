// Browser check of a generated lesson notebook (desargues.lesson.export).
//
// Opens the static Clerk build of dist/lessons/<id>/notebook.clj in headless
// Chromium and checks that
//
// - the page raises no page errors and logs no console errors;
// - every slide title of the lesson is a heading on the page;
// - the integral's KaTeX rendered (a .katex element whose text holds ∫);
// - every <img> loaded (naturalWidth > 0): the embedded PNG/GIF media and
//   the recorded scene's last frame;
// - the :viewer fragment's Mafs mounted (an <svg> inside .MafsView);
// - the recorded scene image (Clerk's _data/*.png) is on the page.
//
// A full-page screenshot goes to dist/lesson-e2e-<id>.png.
//
// Run through `bb lesson:e2e` (builds first), or by hand:
//
//   bb lesson:build lessons/area-under-curve.edn
//   clojure -M:notebooks:lesson-e2e -m desargues.lesson.static-build \
//     dist/lessons/area-under-curve/notebook.clj dist/lesson-e2e/area-under-curve
//   (cd dev/e2e && npm install --no-package-lock)
//   node dev/e2e/lesson_render.mjs lessons/area-under-curve.edn dist/lesson-e2e/area-under-curve
//
// CHROMIUM sets the browser binary; by default the newest Playwright
// headless shell (or Chromium) under ~/.cache/ms-playwright.
// The page loads the emmy-viewers JS bundle and Tailwind from their CDNs, so
// it needs the network.

import { chromium } from "playwright-core";
import { createServer } from "node:http";
import { readFile, stat, mkdir } from "node:fs/promises";
import { existsSync, readdirSync, readFileSync } from "node:fs";
import { join, extname, resolve, dirname } from "node:path";
import { homedir } from "node:os";

const [lessonPath = "lessons/area-under-curve.edn", buildArg] = process.argv.slice(2);
const lessonSrc = readFileSync(lessonPath, "utf8");
const lessonId = (lessonSrc.match(/:lesson\/id\s+:([^\s}]+)/) ?? [])[1] ?? "lesson";
const ROOT = resolve(buildArg ?? `dist/lesson-e2e/${lessonId}`);
const SHOT = resolve(process.env.LESSON_E2E_SHOT ?? `dist/lesson-e2e-${lessonId}.png`);
// Slide titles, read from the lesson EDN (strings after :slide/title).
const TITLES = [...lessonSrc.matchAll(/:slide\/title\s+"((?:[^"\\]|\\.)*)"/g)].map((m) => m[1].replace(/\\(.)/g, "$1"));

function chromiumPath() {
  if (process.env.CHROMIUM) return process.env.CHROMIUM;
  const base = join(homedir(), ".cache/ms-playwright");
  const dirs = existsSync(base) ? readdirSync(base) : [];
  const candidates = [];
  for (const d of dirs.filter((d) => /^chromium_headless_shell-\d+$/.test(d)).sort().reverse())
    for (const sub of ["chrome-headless-shell-linux64", "chrome-linux"])
      candidates.push(join(base, d, sub, "chrome-headless-shell"), join(base, d, sub, "headless_shell"));
  for (const d of dirs.filter((d) => /^chromium-\d+$/.test(d)).sort().reverse())
    for (const sub of ["chrome-linux64", "chrome-linux"]) candidates.push(join(base, d, sub, "chrome"));
  candidates.push("/snap/bin/chromium");
  const hit = candidates.find((p) => existsSync(p));
  if (!hit) throw new Error("No Chromium found; set CHROMIUM");
  return hit;
}

const TYPES = {
  ".html": "text/html", ".js": "text/javascript", ".css": "text/css", ".edn": "application/edn",
  ".json": "application/json", ".svg": "image/svg+xml", ".png": "image/png", ".gif": "image/gif",
  ".woff2": "font/woff2", ".woff": "font/woff",
};

function serve(root) {
  const server = createServer(async (req, res) => {
    try {
      let p = join(root, decodeURIComponent(new URL(req.url, "http://x").pathname));
      if (!p.startsWith(root)) throw new Error("outside root");
      if ((await stat(p)).isDirectory()) p = join(p, "index.html");
      const body = await readFile(p);
      res.writeHead(200, { "content-type": TYPES[extname(p)] ?? "application/octet-stream" });
      res.end(body);
    } catch {
      res.writeHead(404);
      res.end();
    }
  });
  return new Promise((ok) => server.listen(0, "127.0.0.1", () => ok(server)));
}

const failures = [];
const check = (ok, msg) => { console.log(`${ok ? "ok  " : "FAIL"} ${msg}`); if (!ok) failures.push(msg); };

async function main() {
  if (!existsSync(join(ROOT, "index.html"))) throw new Error(`no static build at ${ROOT}`);
  check(TITLES.length > 0, `${TITLES.length} slide titles read from ${lessonPath}`);
  const server = await serve(ROOT);
  const origin = `http://127.0.0.1:${server.address().port}`;
  const exe = chromiumPath();
  console.log(`     chromium: ${exe}`);
  const browser = await chromium.launch({
    executablePath: exe,
    args: ["--use-angle=swiftshader", "--enable-unsafe-swiftshader", "--ignore-gpu-blocklist"],
  });
  try {
    const page = await browser.newPage({ viewport: { width: 1280, height: 900 } });
    const pageErrors = [], consoleErrors = [], failedRequests = [];
    page.on("crash", () => console.log("     PAGE CRASHED"));
    page.on("pageerror", (e) => { pageErrors.push(String(e)); if (process.env.LESSON_E2E_DEBUG) console.log(e.stack); });
    page.on("console", (m) => { if (m.type() === "error") consoleErrors.push(m.text()); });
    page.on("requestfailed", (r) => failedRequests.push(`${r.url().slice(0, 120)} (${r.failure()?.errorText})`));

    await page.goto(`${origin}/`, { waitUntil: "load", timeout: 120000 });
    // Clerk renders client-side: wait for the last slide heading and Mafs.
    await page.waitForFunction(
      (last) => [...document.querySelectorAll("h1,h2,h3")].some((h) => h.textContent.trim() === last)
        && document.querySelector(".katex"),
      TITLES[TITLES.length - 1], { timeout: 120000 }).catch(() => {});
    await page.waitForSelector(".MafsView svg", { timeout: 30000 }).catch(() => {});
    await page.waitForFunction(
      () => [...document.images].every((i) => i.complete), null, { timeout: 30000 }).catch(() => {});
    await page.waitForTimeout(1500);

    await mkdir(dirname(SHOT), { recursive: true });
    await page.screenshot({ path: SHOT, fullPage: true });

    const seen = await page.evaluate(() => ({
      headings: [...document.querySelectorAll("h1,h2,h3,h4")].map((h) => h.textContent.trim()),
      katex: [...document.querySelectorAll(".katex")].map((k) => k.textContent),
      images: [...document.images].map((i) => ({
        src: i.getAttribute("src")?.slice(0, 40) ?? "", w: i.naturalWidth, h: i.naturalHeight,
      })),
      mafs: document.querySelectorAll(".MafsView").length,
      mafsSvg: document.querySelectorAll(".MafsView svg").length,
      mafsPaths: document.querySelectorAll(".MafsView svg path").length,
    }));

    for (const t of TITLES) check(seen.headings.includes(t), `slide heading "${t}"`);
    const integrals = seen.katex.filter((t) => t.includes("∫"));
    check(integrals.length > 0, `KaTeX integral rendered (${seen.katex.length} .katex elements, ${integrals.length} with ∫)`);
    const kinds = (prefix) => seen.images.filter((i) => i.src.startsWith(prefix));
    const pngs = kinds("data:image/png"), gifs = kinds("data:image/gif");
    check(pngs.length >= 2 && pngs.every((i) => i.w > 0),
          `embedded PNG media loaded (${pngs.map((i) => `${i.w}x${i.h}`).join(", ") || "none"})`);
    check(gifs.length >= 1 && gifs.every((i) => i.w > 0),
          `embedded GIF media loaded (${gifs.map((i) => `${i.w}x${i.h}`).join(", ") || "none"})`);
    check(seen.images.length > 0 && seen.images.every((i) => i.w > 0),
          `every <img> loaded, naturalWidth > 0 (${seen.images.length} images)`);
    check(seen.mafs >= 1 && seen.mafsSvg >= 1,
          `Mafs viewer mounted (${seen.mafs} .MafsView, ${seen.mafsSvg} svg, ${seen.mafsPaths} paths)`);
    const scene = seen.images.filter((i) => i.src.startsWith("_data/") || i.src.includes("/_data/"));
    check(scene.length >= 1 && scene.every((i) => i.w > 0),
          `recorded scene image present (${scene.map((i) => `${i.src}… ${i.w}x${i.h}`).join(", ") || "none"})`);
    check(pageErrors.length === 0, `no page errors${pageErrors.length ? ": " + pageErrors.join(" | ") : ""}`);
    check(consoleErrors.length === 0, `no console errors${consoleErrors.length ? ": " + consoleErrors.join(" | ") : ""}`);
    if (failedRequests.length) console.log(`     failed requests: ${failedRequests.join(" | ")}`);
  } finally {
    await browser.close();
    server.close();
  }
  console.log(failures.length ? `\n${failures.length} check(s) failed` : "\nall checks passed");
  console.log(`screenshot: ${SHOT}`);
  process.exit(failures.length ? 1 : 0);
}

main().catch((e) => { console.error(e); process.exit(1); });
