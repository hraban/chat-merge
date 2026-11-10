const log = document.getElementById("log");
const status = document.getElementById("s");
// Twitch's default name colors, for chatters who haven't picked their own.
// Each name gets a random one on first sight and keeps it until reload.
const palette = ["#ff0000", "#0000ff", "#008000", "#b22222", "#ff7f50",
  "#9acd32", "#ff4500", "#2e8b57", "#daa520", "#d2691e", "#5f9ea0",
  "#1e90ff", "#ff69b4", "#8a2be2", "#00ff7f"];
const colors = new Map();
const colorOf = (n) => {
  if (!colors.has(n))
    colors.set(n, palette[Math.floor(Math.random() * palette.length)]);
  return colors.get(n);
};
const es = new EventSource("events");
es.onopen = () => status.textContent = "live";
es.onerror = () => status.textContent = "reconnecting…";
es.onmessage = (e) => {
  const m = JSON.parse(e.data);
  // If you want to only stick when already at the bottom:
  // const stick = innerHeight + scrollY >= document.body.scrollHeight - 40;
  // Instead, because of OBS lack of scrolling, just always stick:
  const stick = true;
  const div = document.createElement("div");
  div.className = "m " + m.platform;
  const p = document.createElement("span");
  p.className = "p";
  p.textContent = "● ";
  // Text goes in as text nodes, never parsed as HTML. Emote ids are
  // restricted to [A-Za-z0-9_] by the server.
  const name = document.createElement("span");
  name.className = "n";
  // The chatter's own Twitch color if they set one, else a random one.
  name.style.color = m.color ?? colorOf(m.author);
  name.textContent = m.author;
  if (m.kind === "action") div.append(p, "* ", name, " ");
  else div.append(p, name, ": ");
  for (const s of m.segments) {
    if (s.emote === undefined) { div.append(s.text); continue; }
    const img = document.createElement("img");
    img.src = "https://static-cdn.jtvnw.net/emoticons/v2/" +
      encodeURIComponent(s.emote) + "/default/dark/1.0";
    img.alt = img.title = s.text;
    div.append(img);
  }
  log.append(div);
  while (log.childElementCount > 1000) log.firstChild.remove();
  if (stick) scrollTo(0, document.body.scrollHeight);
};
