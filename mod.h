#pragma once

#include "typedefs.h"

#include "game/human.h"
#include "game/tool.h"

tool define_tool();

human get_human(id id);

void change_human_health(id id, f64 health);
