final: prev: {
  picom-jonaburg =
    prev.picom.overrideAttrs (old: { src = final.inputs.picom-jonaburg; });
}
