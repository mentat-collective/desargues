// Headless paint check for the browser host. Serves browser/public on a
// local port, opens every demo (and the recorded pendulum scene) in headless
// Chromium through Playwright, seeks to a few times and asserts the stage
// canvas painted non-background pixels with no page or tick errors; one slider
// move must change the picture too, and on the 3D page a mouse drag on empty
// space must orbit the camera.
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
  .concat([{ name: "space (3D, orbit)", query: "?demo=space", orbit: true },
           { name: "pendulum (recorded EDN)", query: "?scene=pendulum" }]);

// CHROME=<path> runs a browser already on disk (e.g. a cached headless shell)
// instead of the one this Playwright release expects.
const browser = await chromium.launch(process.env.CHROME ? { executablePath: process.env.CHROME } : {});
let failed = 0;
try {
  for (const { name, query, orbit } of pages) {
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
    // A 3D page: a real mouse drag on empty canvas (bottom-left corner) must
    // orbit the camera (status().yaw leaves null) and repaint.
    let orbited = true;
    if (orbit) {
      const box = await page.$eval("canvas[data-desargues=stage]", (e) => {
        const r = e.getBoundingClientRect(); return [r.left, r.top, r.width, r.height];
      });
      const [x, y] = [box[0] + 0.05 * box[2], box[1] + 0.92 * box[3]];
      const before = await page.evaluate(() => document.querySelector("canvas").toDataURL());
      await page.mouse.move(x, y);
      await page.mouse.down();
      await page.mouse.move(x + 120, y - 30, { steps: 6 });
      await page.mouse.up();
      const after = await page.evaluate(() => document.querySelector("canvas").toDataURL());
      const yaw = await page.evaluate(() => desargues.browser.main.status().yaw);
      orbited = typeof yaw === "number" && before !== after;
    }
    const ok = painted.every((n, i) => n > 500 || tex[i] > 0) && !tickError && errors.length === 0 &&
               moved && orbited;
    if (!ok) failed++;
    console.log(`${ok ? "PASS" : "FAIL"} ${name}: painted ${painted.join(" / ")} px, tex labels ${tex.join(" / ")}` +
                `${moved ? "" : ", slider did not repaint"}` +
                `${orbited ? (orbit ? ", mouse drag orbited" : "") : ", mouse drag did not orbit"}` +
                `${tickError ? ", tick error " + tickError : ""}` +
                `${errors.length ? ", errors " + errors.join("; ") : ""}`);
    if (process.env.SHOTS) await page.screenshot({ path: `${process.env.SHOTS}/${name.split(" ")[0]}.png` });
    await page.close();
  }
  failed += await checkWidgets(browser, base);
  failed += await checkCalculus(browser, base);
} finally {
  await browser.close();
  server.close();
}
console.log(failed ? `${failed} page(s) failed` : "all pages paint");
process.exit(failed ? 1 : 0);

// Widget kinds (desargues.player.controls): open each widget demo
// (desargues.studio.widget-demos) and act on every widget through its
// data-control attribute the way a user would: click a button, choose an
// option, drag a slider, tick a toggle, type a formula and press Enter. Each
// act must repaint the stage; a refused formula must show its message beside
// the field and leave the picture alone. Returns the number of failed demos.
async function checkWidgets(browser, base) {
  const at = (id) => `[data-control="${id}"]`;
  const picture = (page) => page.evaluate(() => {
    const c = document.querySelector("canvas[data-desargues=stage]");
    return c.toDataURL() + c.parentElement.innerHTML;
  });
  const type = (page, id, text) => page.fill(at(id), text)
    .then(() => page.press(at(id), "Enter"))
    .then(() => page.$eval(at(id), (e) => e.blur()));
  // [what, data-control id, expected element, the act]
  const demos = {
    turing: [
      ["button Step", "step", "BUTTON", (p) => p.click(at("step"))],
      ["button Step", "step", "BUTTON", (p) => p.click(at("step"))],
      ["button Run to halt", "run", "BUTTON", (p) => p.click(at("run"))],
      ["button Reset", "reset", "BUTTON", (p) => p.click(at("reset"))]],
    kepler: [
      ["select solid", "solid", "SELECT",
       (p) => p.selectOption(at("solid"), { label: "dodecahedron (Mars / Earth)" })],
      ["slider turn", "turn", "INPUT range", (p) => p.$eval(at("turn"), (e) => {
        e.value = String(Number(e.min) + 0.7 * (Number(e.max) - Number(e.min)));
        e.dispatchEvent(new Event("input"));
      })],
      ["toggle spheres", "spheres", "INPUT checkbox", (p) => p.click(at("spheres"))]],
    goedel: [
      ["text formula", "formula", "INPUT text", (p) => type(p, "formula", "(Ex)(x=s0)")]],
  };
  let failed = 0;
  for (const [demo, acts] of Object.entries(demos)) {
    const page = await browser.newPage({ viewport: { width: 1300, height: 640 } });
    const errors = [];
    page.on("pageerror", (e) => errors.push(String(e)));
    await page.goto(base + `?demo=${demo}`);
    await page.waitForFunction(() => window.desargues && desargues.browser.main.status().t !== null,
                               null, { timeout: 15000 });
    await page.evaluate(() => desargues.browser.main.seek(99));
    const notes = [];
    let ok = true;
    for (const [label, id, kind, act] of acts) {
      const el = await page.$eval(at(id),(e) => e.tagName + (e.tagName === "INPUT" ? " " + e.type : ""));
      const before = await picture(page);
      await act(page);
      const moved = before !== (await picture(page));
      ok = ok && moved && el === kind;
      notes.push(`${label} ${el === kind ? "" : "(found " + el + ") "}${moved ? "repaints" : "DID NOT repaint"}`);
    }
    if (demo === "goedel") {
      // A symbol outside the alphabet is refused: message shown, picture kept.
      const before = await picture(page);
      await type(page, "formula", "0=q");
      const kept = before === (await picture(page));
      const msg = await page.$eval(at("formula"), (e) => {
        const err = e.nextElementSibling;
        return err && err.style.display !== "none" ? err.textContent : "";
      });
      ok = ok && kept && msg !== "";
      notes.push(`refused "0=q" ${kept ? "kept the picture" : "REPAINTED"}, message "${msg}"`);
    }
    const status = await page.evaluate(() => desargues.browser.main.status());
    ok = ok && !status.tickError && errors.length === 0 && status.painted > 500;
    if (!ok) failed++;
    console.log(`${ok ? "PASS" : "FAIL"} widgets ${demo}: ${notes.join(", ")}` +
                `${status.tickError ? ", tick error " + status.tickError : ""}` +
                `${errors.length ? ", errors " + errors.join("; ") : ""}`);
    if (process.env.SHOTS) await page.screenshot({ path: `${process.env.SHOTS}/widgets-${demo}.png` });
    await page.close();
  }
  return failed;
}

// The calculus demo (?demo=calculus, desargues.player.calculus-demos): pick
// another surface in the f select and move the x0 slider through their
// data-control attributes. Each must repaint the stage and rewrite the live
// readout (f, df/dx, df/dy and their values at the point), which the page
// exposes as desargues.browser.main.readout(). Returns 1 on failure.
async function checkCalculus(browser, base) {
  const at = (id) => `[data-control="${id}"]`;
  const page = await browser.newPage({ viewport: { width: 1300, height: 640 } });
  const errors = [];
  page.on("pageerror", (e) => errors.push(String(e)));
  await page.goto(base + "?demo=calculus");
  await page.waitForFunction(() => window.desargues && desargues.browser.main.status().t !== null,
                             null, { timeout: 15000 });
  await page.evaluate(() => desargues.browser.main.seek(99));
  const state = () => page.evaluate(() => [document.querySelector("canvas[data-desargues=stage]").toDataURL(),
                                           desargues.browser.main.readout()]);
  const notes = [];
  let ok = true;
  const acts = [
    ["select f = saddle", (p) => p.selectOption(at("f"), { label: "saddle" })],
    ["slider x0", (p) => p.$eval(at("x0"), (e) => {
      e.value = String(Number(e.min) + 0.2 * (Number(e.max) - Number(e.min)));
      e.dispatchEvent(new Event("input"));
    })]];
  for (const [label, act] of acts) {
    const [pic0, txt0] = await state();
    await act(page);
    const [pic1, txt1] = await state();
    const repainted = pic0 !== pic1, rewritten = txt0 !== txt1 && txt1.includes("df/dx");
    ok = ok && repainted && rewritten;
    notes.push(`${label} ${repainted ? "repaints" : "DID NOT repaint"}, ` +
               `${rewritten ? "readout now \"" + txt1.split("\n")[1] + "\"" : "readout DID NOT change"}`);
  }
  const status = await page.evaluate(() => desargues.browser.main.status());
  ok = ok && !status.tickError && errors.length === 0 && status.painted > 500;
  console.log(`${ok ? "PASS" : "FAIL"} calculus: ${notes.join(", ")}, painted ${status.painted} px` +
              `${status.tickError ? ", tick error " + status.tickError : ""}` +
              `${errors.length ? ", errors " + errors.join("; ") : ""}`);
  if (process.env.SHOTS) await page.screenshot({ path: `${process.env.SHOTS}/calculus.png` });
  await page.close();
  return ok ? 0 : 1;
}
