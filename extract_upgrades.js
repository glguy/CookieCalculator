JSON.stringify(Game.UpgradesById.map(function(x) { return { name: x.name, pool: x.pool, power: x.power, price: x.basePrice, icon: x.icon } }))
