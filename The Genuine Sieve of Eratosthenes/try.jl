import Dates

function find_primes(max=70663)
    numbers = [true for _ in 1:max]
    for i in 2:max
        if numbers[i]
            for j in i+1:max
                if j % i == 0
                    numbers[j] = false
                end
            end
        end
    end
    return [i for i in 1:max if numbers[i]][end]
end

t0 = Dates.now()
n = find_primes()
t1 = Dates.now()
println(t1 - t0)
println(n)