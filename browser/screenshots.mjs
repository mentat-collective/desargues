// Browser screenshots for the README gallery (doc/showcase/browser-*.png):
// serves browser/public like check.mjs, opens a few demo pages in headless
// Chromium through Playwright, sets their widgets to an interesting state the
// way a user would, seeks to the end and captures the page (stage + widgets).
//
//   clojure -M:browser                      # builds browser/public/js/main.js
//   cd browser && npm i --no-save --no-package-lock --ignore-scripts playwright
//   CHROME=<chrome-headless-shell> node screenshots.mjs [out-dir]
//
// out-dir defaults to ../doc/showcase. Adding a shot is one entry in `shots`.
import http from "node:http";
import fs from "node:fs";
import path from "node:path";
import { fileURLToPath } from "node:url";
import { chromium } from "playwright";

const here = path.dirname(fileURLToPath(import.meta.url));
const root = path.join(here, "public");
const out = path.resolve(process.argv[2] || path.join(here, "..", "doc", "showcase"));
const types = { ".html": "text/html", ".js": "text/javascript", ".edn": "application/edn" };

const server = http.createServer((req, res) => {
  const url = new URL(req.url, "http://x");
  const file = path.join(root, url.pathname === "/" ? "index.html" : url.pathname);
  if (!file.startsWith(root) || !fs.existsSync(file)) { res.writeHead(404); return res.end(); }
  res.writeHead(200, { "content-type": types[path.extname(file)] || "application/octet-stream" });
  fs.createReadStream(file).pipe(res);
});
await new Promise((r) => server.listen(0, "127.0.0.1", r));
const base = `http://127.0.0.1:${server.address().port}/`;

const at = (id) => `[data-control="${id}"]`;
const slide = (id, frac) => (p) => p.$eval(at(id), (e, frac) => {
  e.value = String(Number(e.min) + frac * (Number(e.max) - Number(e.min)));
  e.dispatchEvent(new Event("input"));
}, frac);

// name -> page query and the widget acts that set its state
const shots = [
  { name: "browser-kepler", query: "?demo=kepler",
    acts: [(p) => p.selectOption(at("solid"), { label: "icosahedron (Earth / Venus)" }),
           slide("turn", 0.15)] },
  { name: "browser-turing", query: "?demo=turing",
    acts: [(p) => p.click(at("step")), (p) => p.click(at("step")), (p) => p.click(at("step")),
           (p) => p.click(at("step")), (p) => p.click(at("step"))] },
  { name: "browser-goedel", query: "?demo=goedel",
    acts: [async (p) => { await p.fill(at("formula"), "0=0|~(0=s0)"); await p.press(at("formula"), "Enter"); }] },
  { name: "browser-plot", query: "?demo=plot",
    acts: [slide("freq", 0.55), slide("amp", 0.6)] },
];

const browser = await chromium.launch(process.env.CHROME ? { executablePath: process.env.CHROME } : {});
try {
  fs.mkdirSync(out, { recursive: true });
  for (const { name, query, acts } of shots) {
    const page = await browser.newPage({ viewport: { width: 1300, height: 640 } });
    await page.goto(base + query);
    await page.waitForFunction(() => window.desargues && desargues.browser.main.status().t !== null,
                               null, { timeout: 15000 });
    for (const act of acts) await act(page);
    await page.evaluate(() => desargues.browser.main.seek(99));
    await page.evaluate(() => document.activeElement && document.activeElement.blur());
    const file = path.join(out, `${name}.png`);
    await page.screenshot({ path: file, fullPage: true });
    console.log("wrote", file);
    await page.close();
  }
} finally {
  await browser.close();
  server.close();
}
