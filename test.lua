if a == 23 and (f == 43 or (g == 32 and b == 34 and 1) == 1) then
    print('yes')
else
    print('no')
end

--[[
function x(test1, test2)
    function y(...)
        print(...)
    end

    x = 32
    y("hello")
    y(true)
    y(3.4)
end


x('abc', false)
]]