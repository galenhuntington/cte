
--  Number of leap seconds thru 2020-12-31.
cur_leaps = 37

--  Returns current CTE time.
--  Optional argument gives a time, but then specify leaps.
function cte (date, leaps)
	if not date then date = os.time() end
	if not leaps then leaps = cur_leaps end
	--  Ticks since CTE epoch (2000 CTE).
	local ticks = 444911616 + (date + leaps - 10957*86400) * 13824000
	print(ticks)
	local x = ticks
	local units = {}
	for _, u in ipairs({233755, 60, 60, 60, 24, 30, 12}) do
		table.insert(units, 1, x % u)
		x = math.floor(x / u)
		end
	table.remove(units)
	table.insert(units, 1, x)
	for i, a in ipairs({2000, 1, 1}) do
		units[i] = units[i] + a end
	return units
	end

