
# The array of ligts will be a Matrix, where -1 means light is off
# +1 means light is on. Therefore:
#  - Turn on => add 2, limit to max of +1
#  - Turn 0ff => subtract 2, limit to a minimum of -1
#  - Toggle = multiply by -1
#
# And it can all be done with masks, e.g. turn on a portion involves
# making a mask array with 2s where we want to turn on and 0s where we
# don't, then adding that mask to the current array and limiting

struct point
    x :: Int
    y :: Int
end

struct action
    name :: String
    combination_fn :: Function
    mask :: Matrix{Int}
end

# Functions for parsing the actions
function parse_point(s) 
    fst, lst = split(s, ",")
    point(parse(Int, fst) + 1, parse(Int, lst) + 1)
end

function extract_action(s) 
    if occursin("turn on", s)
        "turn on"
    elseif occursin("turn off", s)
        "turn off"
    elseif occursin("toggle", s)
        "toggle"
    else
        error("action not found")
    end
end

function get_fst_points(s, action)
    replace(s, "$action " => "")
end

function make_mask(x, y, p1, p2, active_value, identity_value) 
    base = ones(Int, x, y) .* identity_value
    base[p1.x:p2.x, p1.y:p2.y] .= active_value
    base
end

function make_action(s, p1, p2) 
    if s == "turn on"
        action("Turn On", .+, make_mask(1000, 1000, p1, p2, 2, 0))
    elseif s == "turn off"
        action("Turn Off", .+, make_mask(1000, 1000, p1, p2, -2, 0))
    elseif s == "toggle"
        action("Toggle", .*, make_mask(1000, 1000, p1, p2, -1, 1))
    else
        error("Unknown action")
    end
end

function make_action2(s, p1, p2) 
    if s == "turn on"
        action("Turn On", .+, make_mask(1000, 1000, p1, p2, 1, 0))
    elseif s == "turn off"
        action("Turn Off", .+, make_mask(1000, 1000, p1, p2, -1, 0))
    elseif s == "toggle"
        action("Toggle", .+, make_mask(1000, 1000, p1, p2, 2, 0))
    else
        error("Unknown action")
    end
end

function parse_action(s, make_action_fn) 
    fst, lst_points = split(s, " through ")
    
    action = extract_action(fst)
    fst_points = get_fst_points(fst, action)

    make_action_fn(action, parse_point(fst_points), parse_point(lst_points))
end


function cap_matrix(mat)
    I,J = size(mat)
    for i in 1:I
        for j in 1:J
            mat[i,j] = max(-1, min(mat[i,j], 1))
        end
    end
    mat
end
    

function cap_matrix2(mat)
    I,J = size(mat)
    for i in 1:I
        for j in 1:J
            mat[i,j] = max(0, mat[i,j])
        end
    end
    mat
end
    
function init_array(x, y, value)
    arr = zeros(Int, x, y)
    arr .+ value
end

# 569999
function part1()
    action_strs = readlines("../inputs/6_input.txt")
    base = init_array(1000, 1000, -1)
    for action_str in action_strs
        action = parse_action(action_str, make_action)
        base = action.combination_fn(base, action.mask)
        cap_matrix(base)
    end
    count(==(1), base)
end

# 17836115
function part2()
    action_strs = readlines("../inputs/6_input.txt")
    base = init_array(1000, 1000, 0)
    for action_str in action_strs
        action = parse_action(action_str, make_action2)
        base = action.combination_fn(base, action.mask)
        cap_matrix2(base)
    end
    sum(base)
end
