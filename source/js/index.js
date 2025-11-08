import Main from "../elm/Main.elm";

const settingsRaw = localStorage.getItem("settings");
const settings = settingsRaw ? JSON.parse(settingsRaw) : {};

const app = Main.init({
  flags: {
    viewport: { width: window.innerWidth, height: window.innerHeight },
    languages: {
      default: navigator.languages ?? [navigator.language],
      selected: settings?.language,
    },
  },
});

app.ports.sendToJs.subscribe(({ msg, value }) => {
  switch (msg) {
    case "saveSettings":
      localStorage.setItem("settings", JSON.stringify(value));
  }
});
