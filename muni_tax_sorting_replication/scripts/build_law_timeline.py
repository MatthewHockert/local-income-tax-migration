#!/usr/bin/env python3
# build_law_timeline.py -- Python (matplotlib) twin of build_law_timeline.R.
# Background figure: timeline of the PA local-tax statutes (EIT + property) that
# generate or constrain the variation, with the estimation window shaded.
# Output: figures/law_timeline_py.png
import os
import matplotlib
matplotlib.use("Agg")
import matplotlib.pyplot as plt
from matplotlib.patches import Rectangle

ROOT = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
FIG_DIR = os.path.join(ROOT, "figures")

# year, name, desc, ypos (vertical stagger), key (bold the binding date)
events = [
    (1932, "Sterling Act",     "Phila. wage tax",                                  1.0, False),
    (1965, "Act 511",          "Local Tax Enabling Act\n(local EIT; 0.5% cap)",   -1.0, False),
    (1972, "Home Rule",        "Charter Law\n(may exceed cap)",                    1.0, False),
    (1987, "Act 47",           "Distressed cities\n(may exceed cap)",             -1.0, False),
    (2006, "Act 1",            "Taxpayer Relief Act\n(SD EIT shift; property\nanti-windfall)", 2.4, False),
    (2008, "Act 32 enacted",   "consolidated\nwithholding",                       -1.6, False),
    (2012, "Act 32 effective", "EIT becomes binding",                              1.4, True),
]

fig, ax = plt.subplots(figsize=(10, 5))

# estimation window
ax.add_patch(Rectangle((2002, -3.2), 2019 - 2002, 6.4, facecolor="0.85",
                       alpha=0.5, edgecolor="none", zorder=0))
ax.text(2010.5, -3.0, "Estimation sample 2002–2019", ha="center", va="bottom",
        style="italic", color="0.35", fontsize=9)

# the timeline spine
ax.axhline(0, color="0.25", linewidth=1.0, zorder=1)

for year, name, desc, ypos, key in events:
    ax.plot([year, year], [0, ypos], color="black", linewidth=0.8, zorder=2)   # connector
    ax.plot(year, 0, "o", color="black", markersize=7, zorder=3)               # point
    ax.text(year, ypos, f"{name} ({year})\n{desc}", ha="center",
            va="bottom" if ypos > 0 else "top", fontsize=8, linespacing=0.95,
            fontweight="bold" if key else "normal",
            bbox=dict(boxstyle="round,pad=0.2", facecolor="white", edgecolor="none"),
            zorder=4)

ax.set_xlim(1928, 2022)
ax.set_ylim(-3.6, 3.8)
ax.set_xticks(range(1930, 2021, 10))
ax.set_yticks([])
for s in ("top", "left", "right"):
    ax.spines[s].set_visible(False)
ax.spines["bottom"].set_color("0.6")
ax.tick_params(axis="x", colors="0.3")

ax.set_title("Timeline of Pennsylvania local-tax rules", fontweight="bold",
             fontsize=14, loc="left", pad=28)
ax.text(0.0, 1.045, "Earned Income Tax (EIT) and property-tax statutes shaping the design",
        transform=ax.transAxes, fontsize=11, va="bottom")

out = os.path.join(FIG_DIR, "law_timeline_py.png")
fig.savefig(out, dpi=300, bbox_inches="tight")
print("Saved figures/law_timeline_py.png")
