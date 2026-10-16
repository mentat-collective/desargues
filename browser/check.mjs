// Headless paint check for the browser host. Serves browser/public on a
// local port, opens every demo (and the recorded pendulum scene) in headless
// Chromium through Playwright, seeks to a few times and asserts the stage
// canvas painted non-background pixels with no page or tick errors; one slider
// move must change the picture too.
//
//   cd browser && npm i --no-save --no-package-lock playwright && node check.mjs
//
// Exit code 0 when every page paints, 1 otherwise.
import http from "node:http";
import fs from "node:fs";
import path from "node:path";
import { fileURLToPath } from "node:url";
import { chromium } from "playwright";

const root = path.join(path.dirname(fileURLToPath(import.meta.url)), "public");
const types = { ".html": "text/html", ".js": "text/javascript", ".edn": "application/edn" };

const server = http.createServer((req, res) => {
  const url = new URL(req.url, "http://x");
  // KaTeX's CDN is unreachable offline: the host falls back to canvas text.
  const file = path.join(root, url.pathname === "/" ? "index.html" : url.pathname);
  if (!file.startsWith(root) || !fs.existsSync(file)) { res.writeHead(404); return res.end(); }
  res.writeHead(200, { "content-type": types[path.extname(file)] || "application/octet-stream" });
  fs.createReadStream(file).pipe(res);
});
await new Promise((r) => server.listen(0, "127.0.0.1", r));
const base = `http://127.0.0.1:${server.address().port}/`;

const pages = ["axes", "plot", "parametric", "polar", "field", "tangent", "riemann", "tex"]
  .map((d) => ({ name: d, query: `?demo=${d}` }))
  .concat([{ name: "pendulum (recorded EDN)", query: "?scene=pendulum" }]);

// CHROME=<path> runs a browser already on disk (e.g. a cached headless shell)
// instead of the one this Playwright release expects.
const browser = await chromium.launch(process.env.CHROME ? { executablePath: process.env.CHROME } : {});
let failed = 0;
try {
  for (const { name, query } of pages) {
    const page = await browser.newPage({ viewport: { width: 1300, height: 640 } });
    const errors = [];
    page.on("pageerror", (e) => errors.push(String(e)));
    await page.goto(base + query);
    await page.waitForFunction(() => window.desargues && desargues.browser.main.status().t !== null,
                               null, { timeout: 15000 });
    const results = [];
    for (const t of [1.5, 3.0, 99]) {
      results.push(await page.evaluate((t) => desargues.browser.main.seek(t), t));
    }
    const painted = results.map((r) => r.painted);
    const tex = results.map((r) => r.tex);
    const tickError = results.map((r) => r.tickError).find(Boolean);
    let moved = true;
    if (query.startsWith("?demo=")) {
      const ctl = await page.$eval("input[type=range]", (e) => [e.dataset.control, e.min, e.max]);
      const v = Number(ctl[1]) + 0.83 * (Number(ctl[2]) - Number(ctl[1]));
      const before = await page.evaluate(() => document.querySelector("canvas").toDataURL() + document.querySelector("#stage").innerHTML);
      await page.evaluate(([k, v]) => desargues.browser.main.set_param(k, v), [ctl[0], v]);
      const after = await page.evaluate(() => document.querySelector("canvas").toDataURL() + document.querySelector("#stage").innerHTML);
      moved = before !== after;
    }
    const ok = painted.every((n, i) => n > 500 || tex[i] > 0) && !tickError && errors.length === 0 && moved;
    if (!ok) failed++;
    console.log(`${ok ? "PASS" : "FAIL"} ${name}: painted ${painted.join(" / ")} px, tex labels ${tex.join(" / ")}` +
                `${moved ? "" : ", slider did not repaint"}` +
                `${tickError ? ", tick error " + tickError : ""}` +
                `${errors.length ? ", errors " + errors.join("; ") : ""}`);
    if (process.env.SHOTS) await page.screenshot({ path: `${process.env.SHOTS}/${name.split(" ")[0]}.png` });
    await page.close();
  }
} finally {
  await browser.close();
  server.close();
}
console.log(failed ? `${failed} page(s) failed` : "all pages paint");
process.exit(failed ? 1 : 0);
