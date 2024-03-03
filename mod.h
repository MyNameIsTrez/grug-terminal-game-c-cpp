#pragma once

#include "typedefs.h"

#include "game/human.h"
#include "game/tool.h"

struct tool define_tool();

f64 get_human_health();
f64 get_human_max_health();
void set_human_health(f64 health);

int min_i32(i32 a, i32 b);
