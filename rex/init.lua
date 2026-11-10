-- Rex config, symlinked to ~/.config/rex/init.lua by config/mise/mise.toml.
-- Apply changes with `rex config reload`.

-- Cmd keys that go to Emacs when it is the focused pane's foreground process,
-- and run Rex's default action everywhere else. Emacs sees them as `s-' keys
-- through the kitty keyboard protocol (kkp in emacs/main.el).
local emacs_cmd_keys = {
  ["cmd+d"] = "pane.split.right",
  ["cmd+f"] = "client.find.open",
  ["cmd+g"] = "client.find.next",
  ["cmd+k"] = "session.switch",
  ["cmd+p"] = "pane.go_to",
}

local terminal = "com.superlogical.terminal"

local function in_emacs(block_id)
  local p = rex.block.call(terminal, "process", { block_id = block_id })
  local fg = p and p.foreground
  -- The binary is Emacs.app/Contents/MacOS/Emacs; plain builds are emacs.
  return fg ~= nil and fg.name:lower() == "emacs"
end

-- Kitty keyboard protocol encoding of a key such as "cmd+d": CSI code;mods u,
-- where mods is 1 + shift(1) + alt(2) + ctrl(4) + super(8). Written by hand
-- because pane.send_key does not deliver Cmd to Emacs as super.
local mod_bits = { shift = 1, alt = 2, ctrl = 4, cmd = 8 }

local function kitty_bytes(key)
  local mods, char = 1, nil
  for part in key:gmatch("[^+]+") do
    if mod_bits[part] then mods = mods + mod_bits[part] else char = part end
  end
  return string.format("\27[%d;%du", char:byte(), mods) -- ASCII keys only
end

-- An action, not a key function: Rex refuses rex.client.queue from a key
-- function, even through rex.invoke.
rex.action{
  name  = "emacs_or_default",
  title = "Send Key to Emacs or Run Default",
  args  = { key = "string", action = "string" },
  run   = function(ctx, args)
    if in_emacs(ctx.block_id) then
      rex.block.call(terminal, "write", { block_id = ctx.block_id, data = kitty_bytes(args.key) })
    else
      rex.client.queue(args.action)
    end
  end,
}

for key, action in pairs(emacs_cmd_keys) do
  rex.bind(key, "emacs_or_default", { key = key, action = action })
end
