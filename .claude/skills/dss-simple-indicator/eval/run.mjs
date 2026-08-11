#!/usr/bin/env node
// Skill-routing eval for dss-simple-indicator.
//
// Fires many headless Claude Code sessions (via the `claude -p` CLI) against
// a fixed prompt set, watches the FIRST tool call each session makes, and
// kills the process the instant that's captured -- no R script ever runs,
// no file ever gets written. Reports what fraction of "should trigger"
// prompts actually invoke Skill(skill="dss-simple-indicator") as the first
// move, and what fraction of "should not trigger" prompts stay away from it.
//
// Usage:
//   node .claude/skills/dss-simple-indicator/eval/run.mjs [options]
//
// Options:
//   --trials-positive <n>   trials per positive prompt (default 5)
//   --trials-negative <n>   trials per negative prompt (default 3)
//   --concurrency <n>       parallel sessions (default 6)
//   --model <name>          model id (default claude-sonnet-5)
//   --bin <path>            claude binary (default $CLAUDE_CODE_EXECPATH or "claude")
//   --case <id>             only run this one case id (repeatable)
//
// Requires a Claude Code build with Skills support (2.x+). The `claude` on
// PATH may be an older build without the Skill tool -- if so, set
// CLAUDE_CODE_EXECPATH (or pass --bin) to a newer native binary, e.g. the
// one bundled with the VS Code extension:
//   ~/.vscode/extensions/anthropic.claude-code-*/resources/native-binary/claude

import { spawn } from "node:child_process";
import { createInterface } from "node:readline";
import { readFileSync, writeFileSync, mkdirSync, existsSync } from "node:fs";
import { fileURLToPath } from "node:url";
import path from "node:path";

const __dirname = path.dirname(fileURLToPath(import.meta.url));
const REPO_ROOT = path.resolve(__dirname, "..", "..", "..", "..");
const SKILL_NAME = "dss-simple-indicator";

const DISALLOWED_TOOLS =
  "Bash,Write,Edit,MultiEdit,NotebookEdit,WebFetch,WebSearch";
const PER_TRIAL_TIMEOUT_MS = 75_000;

function parseArgs(argv) {
  const opts = {
    trialsPositive: 5,
    trialsNegative: 3,
    concurrency: 6,
    model: "claude-sonnet-5",
    bin: process.env.CLAUDE_CODE_EXECPATH || "claude",
    caseIds: null,
  };
  for (let i = 0; i < argv.length; i++) {
    const a = argv[i];
    if (a === "--trials-positive") opts.trialsPositive = Number(argv[++i]);
    else if (a === "--trials-negative") opts.trialsNegative = Number(argv[++i]);
    else if (a === "--concurrency") opts.concurrency = Number(argv[++i]);
    else if (a === "--model") opts.model = argv[++i];
    else if (a === "--bin") opts.bin = argv[++i];
    else if (a === "--case") (opts.caseIds ??= []).push(argv[++i]);
    else if (a === "--help" || a === "-h") {
      console.log(readFileSync(new URL(import.meta.url), "utf8").split("\n").slice(1, 26).join("\n"));
      process.exit(0);
    }
  }
  return opts;
}

function loadCases() {
  const raw = JSON.parse(readFileSync(path.join(__dirname, "cases.json"), "utf8"));
  const cases = [];
  for (const c of raw.positive) cases.push({ ...c, category: "positive" });
  for (const c of raw.negative) cases.push({ ...c, category: "negative" });
  return cases;
}

// Runs one headless session, resolves as soon as the FIRST tool_use block
// appears anywhere in the stream (or the session ends with no tool use),
// and kills the child immediately so nothing downstream ever executes.
function runTrial({ bin, model, prompt, caseId, trial }) {
  return new Promise((resolve) => {
    const args = [
      "-p",
      prompt,
      "--output-format",
      "stream-json",
      "--verbose",
      "--model",
      model,
      "--disallowedTools",
      DISALLOWED_TOOLS,
    ];
    const start = Date.now();
    const child = spawn(bin, args, { cwd: REPO_ROOT, stdio: ["ignore", "pipe", "pipe"] });

    let settled = false;
    let sawResultWithNoTool = false;
    let resultText = null;

    const finish = (partial) => {
      if (settled) return;
      settled = true;
      clearTimeout(hardTimeout);
      const elapsedMs = Date.now() - start;
      try {
        child.kill("SIGTERM");
        setTimeout(() => {
          try {
            child.kill("SIGKILL");
          } catch {}
        }, 3000);
      } catch {}
      resolve({
        caseId,
        trial,
        elapsedMs,
        tool: null,
        toolInput: null,
        triggered: false,
        status: "no_tool_use",
        resultText,
        ...partial,
      });
    };

    const hardTimeout = setTimeout(() => finish({ status: "timeout" }), PER_TRIAL_TIMEOUT_MS);

    const rl = createInterface({ input: child.stdout });
    rl.on("line", (line) => {
      if (settled || !line.trim()) return;
      let obj;
      try {
        obj = JSON.parse(line);
      } catch {
        return;
      }
      if (obj.type === "assistant") {
        const blocks = obj.message?.content ?? [];
        const toolUse = blocks.find((b) => b.type === "tool_use");
        if (toolUse) {
          const triggered = toolUse.name === "Skill" && toolUse.input?.skill === SKILL_NAME;
          finish({
            tool: toolUse.name,
            toolInput: toolUse.input,
            triggered,
            status: "tool_use",
          });
        }
      } else if (obj.type === "result") {
        sawResultWithNoTool = true;
        resultText = typeof obj.result === "string" ? obj.result.slice(0, 300) : null;
      }
    });

    child.on("close", () => {
      if (!settled) finish({ status: sawResultWithNoTool ? "text_only" : "closed_early" });
    });
    child.on("error", (err) => {
      finish({ status: "spawn_error", resultText: String(err) });
    });
  });
}

// Confirms `bin` actually exposes a Skill tool before burning a whole eval
// run on it. Older Claude Code builds (pre-Skills) have no Skill tool at
// all, so every trial would silently look like a "miss" -- not because the
// model declined to use the skill, but because it was never an option.
function preflightSkillSupport({ bin, model }) {
  return new Promise((resolve) => {
    const child = spawn(
      bin,
      [
        "-p",
        "Reply with the word ok. Do not use any tools.",
        "--output-format",
        "stream-json",
        "--verbose",
        "--model",
        model,
        "--disallowedTools",
        DISALLOWED_TOOLS,
      ],
      { cwd: REPO_ROOT, stdio: ["ignore", "pipe", "pipe"] }
    );

    let settled = false;
    const finish = (result) => {
      if (settled) return;
      settled = true;
      clearTimeout(hardTimeout);
      try {
        child.kill("SIGTERM");
        setTimeout(() => {
          try {
            child.kill("SIGKILL");
          } catch {}
        }, 2000);
      } catch {}
      resolve(result);
    };
    const hardTimeout = setTimeout(() => finish({ ok: false, reason: "timed out waiting for session init" }), 20_000);

    const rl = createInterface({ input: child.stdout });
    rl.on("line", (line) => {
      if (settled || !line.trim()) return;
      let obj;
      try {
        obj = JSON.parse(line);
      } catch {
        return;
      }
      if (obj.type === "system" && obj.subtype === "init") {
        const tools = obj.tools ?? [];
        finish({ ok: tools.includes("Skill"), tools });
      }
    });
    child.on("close", (code) => finish({ ok: false, reason: `process exited (code ${code}) before reporting its tool list` }));
    child.on("error", (err) => finish({ ok: false, reason: String(err) }));
  });
}

async function pool(tasks, concurrency, worker) {
  const results = new Array(tasks.length);
  let next = 0;
  async function runOne() {
    while (next < tasks.length) {
      const i = next++;
      results[i] = await worker(tasks[i]);
      process.stderr.write(
        `[${i + 1}/${tasks.length}] ${tasks[i].caseId} trial ${tasks[i].trial}: ` +
          `${results[i].triggered ? "TRIGGERED" : results[i].tool ?? results[i].status}\n`
      );
    }
  }
  await Promise.all(Array.from({ length: Math.min(concurrency, tasks.length) }, runOne));
  return results;
}

async function main() {
  const opts = parseArgs(process.argv.slice(2));

  let cases = loadCases();
  if (opts.caseIds) cases = cases.filter((c) => opts.caseIds.includes(c.id));
  if (cases.length === 0) {
    console.error("No matching cases.");
    process.exit(1);
  }

  console.error(`Checking that "${opts.bin}" exposes a Skill tool...`);
  const preflight = await preflightSkillSupport({ bin: opts.bin, model: opts.model });
  if (!preflight.ok) {
    console.error(
      `\nERROR: "${opts.bin}" does not expose a Skill tool` +
        (preflight.tools ? ` (tools seen: ${preflight.tools.join(", ")})` : preflight.reason ? ` (${preflight.reason})` : "") +
        `.\n\nThis eval requires a Claude Code build with Skills support (2.x+). The "claude"` +
        `\non PATH may be an older build without it. Point this at a newer binary via` +
        `\n--bin <path> or the CLAUDE_CODE_EXECPATH env var, e.g. the one bundled with the` +
        `\nVS Code extension:` +
        `\n  ~/.vscode/extensions/anthropic.claude-code-*/resources/native-binary/claude\n`
    );
    process.exit(1);
  }
  console.error("OK -- Skill tool available.\n");

  const tasks = [];
  for (const c of cases) {
    const trials = c.category === "positive" ? opts.trialsPositive : opts.trialsNegative;
    for (let t = 1; t <= trials; t++) {
      tasks.push({ caseId: c.id, category: c.category, prompt: c.prompt, trial: t });
    }
  }

  console.error(
    `Running ${tasks.length} trials across ${cases.length} cases ` +
      `(model=${opts.model}, bin=${opts.bin}, concurrency=${opts.concurrency})...\n`
  );

  const results = await pool(tasks, opts.concurrency, (task) =>
    runTrial({ bin: opts.bin, model: opts.model, prompt: task.prompt, caseId: task.caseId, trial: task.trial })
  );

  const byId = new Map(cases.map((c) => [c.id, c]));
  const merged = results.map((r, i) => ({ ...tasks[i], ...r }));

  const outDir = path.join(__dirname, "results");
  mkdirSync(outDir, { recursive: true });
  const stamp = new Date().toISOString().replace(/[:.]/g, "-");
  const rawPath = path.join(outDir, `${stamp}.jsonl`);
  writeFileSync(rawPath, merged.map((r) => JSON.stringify(r)).join("\n") + "\n");

  // Per-case summary
  const perCase = new Map();
  for (const r of merged) {
    if (!perCase.has(r.caseId)) perCase.set(r.caseId, []);
    perCase.get(r.caseId).push(r);
  }

  console.log("\n=== Per-case results ===");
  console.log(
    "id".padEnd(5) + "category".padEnd(11) + "trigger_rate".padEnd(14) + "note"
  );
  for (const [id, rs] of perCase) {
    const rate = rs.filter((r) => r.triggered).length / rs.length;
    const c = byId.get(id);
    console.log(
      id.padEnd(5) +
        c.category.padEnd(11) +
        `${rs.filter((r) => r.triggered).length}/${rs.length} (${(rate * 100).toFixed(0)}%)`.padEnd(14) +
        (c.note ?? "")
    );
  }

  const positiveResults = merged.filter((r) => r.category === "positive");
  const negativeResults = merged.filter((r) => r.category === "negative");
  const positiveRate = positiveResults.filter((r) => r.triggered).length / (positiveResults.length || 1);
  const negativeFalseTriggerRate =
    negativeResults.filter((r) => r.triggered).length / (negativeResults.length || 1);

  console.log("\n=== Summary ===");
  console.log(
    `Positive trigger rate (should call skill):     ${(positiveRate * 100).toFixed(1)}% ` +
      `(${positiveResults.filter((r) => r.triggered).length}/${positiveResults.length})`
  );
  console.log(
    `Negative false-trigger rate (should NOT call):  ${(negativeFalseTriggerRate * 100).toFixed(1)}% ` +
      `(${negativeResults.filter((r) => r.triggered).length}/${negativeResults.length})`
  );
  console.log(`\nRaw trial log: ${path.relative(REPO_ROOT, rawPath)}`);
}

main();
