-- The General Lua Library
-- This set of code is meant to provide basic functionality
-- in order to facilitate the writing of events and
-- other Civilization II Libraries

-- BRIEF DOCUMENTATION
-- More documentation about how these functions work can
-- be found at their definition.

-- Any function here that accepts a tile will also
-- accept a table {[1]=x,[2]=y,[3]=z}, a table 
-- {[1]=x,[2]=y} and assume z = 0, or a table
-- {x=x,y=y,z=z}, or a table {x=x,y=y} and assume
-- z = 0
--
-- LIST OF FUNCTIONS
-- * means planned but not implemented
-- # means needs testing
--
--
--gen.checkBits(integer,string)-->boolean
--gen.setBits(integer,string)-->integer
--gen.printBits(integer,numOfBits or nil) --> string
--gen.isBit1(integer,bitNumber)--> boolean
--gen.isBit0(integer,bitNumber)--> boolean
--gen.setBit1(integer,bitNumber)-->integer
--gen.setBit0(integer,bitNumber)-->integer
--gen.makeThresholdTable(table or nil)-->thresholdTable
--#applyWonderBonus(wonderObject or integer,tribeObject or integer)-->boolean
--#gen.toTile(tile or table)-->tile
--gen.hasIrrigation(tile)-->boolean
--gen.placeIrrigation(tile)-->void
--gen.removeIrrigation(tile)-->void
--*gen.hasMine(tile)-->boolean
--*gen.placeMine(tile)-->void
--*gen.removeMine(tile)-->void
--*gen.hasFarmland(tile)-->boolean
--*gen.placeFarmland(tile)-->void
--*gen.removeFarmland(tile)-->void
--*gen.hasRoad(tile)-->boolean
--*gen.placeRoad(tile)-->void
--*gen.removeRoad(tile)-->void
--*gen.hasRailroad(tile)-->boolean
--*gen.placeRailroad(tile)-->void
--*gen.removeRailroad(tile)-->void
--*gen.hasFortress(tile)-->boolean
--*gen.placeFortress(tile)-->void
--*gen.removeFortress(tile)-->void
--*gen.hasAirbase(tile)-->boolean
--*gen.placeAirbase(tile)-->void
--*gen.removeAirbase(tile)-->void
--*gen.hasPollution(tile)-->boolean
--*gen.placePollution(tile)-->void
--*gen.removePollution(tile)-->void
--*gen.hasTransporter(tile)-->boolean
--*gen.placeTransporter(tile)-->void
--*gen.removeTransporter(tile)-->void
--*gen.isFortifying(unit)-->boolean
--*gen.setToFortifying(unit)-->void
--*gen.isFortified(unit)-->boolean
--*gen.setToFortified(unit)-->void
--*gen.isSleeping(unit)-->boolean
--*gen.setToSleeping(unit)-->void
--*gen.isIrrigating(unit)-->boolean
--*gen.setToIrrigating(unit)-->void
--*gen.isMining(unit)-->boolean
--*gen.setToMining(unit)-->void
--*gen.isTransformingTerrain(unit)-->boolean
--*gen.setToTransformingTerrain(unit)-->void
--*gen.isBuildingAirbase(unit)-->boolean
--*gen.setToBuildingAirbase(unit)-->void
--*gen.isBuildingTransporter(unit)-->boolean
--*gen.setToBuildingTransporter(unit)-->void
--*gen.isGoingTo(unit)-->boolean
--*gen.setToGoingTo(unit,tile)-->void
--*gen.isNoOrder(unit)-->boolean
--*gen.setToNoOrders(unit)-->void
--*gen.isSeeTwoSpaces(unitType)-->boolean
--*gen.giveSeeTwoSpaces(unitType)-->void
--*gen.removeSeeTowSpaces(unitType)-->void
--*gen.isIgnoreZOC(unitType)-->boolean
--*gen.giveIgnoreZOC(unitType)-->void
--*gen.removeIgnoreZOC(unitType)-->void
--*gen.isAmphibious(unitType)-->boolean
--*gen.giveAmpibious(unitType)-->void
--*gen.removeAmphibious(unitType)-->void
--*gen.isSubmarine(unitType)-->boolean
--*gen.giveSubmarine(unitType)-->void
--*gen.removeSubmarine(unitType)-->void
--*gen.isAttackAir(unitType)-->boolean
--*gen.giveAttackAir(unitType)-->void
--*gen.removeAttackAir(unitType)-->void
--#gen.isCoastal(unitType)-->boolean
--*gen.giveCoastal(unitType)-->void
--*gen.removeCoastal(unitType)-->void
--*gen.isIgnoreWalls(unitType)-->boolean
--*gen.giveIngoreWalls(unitType)-->void
--*gen.removeIgnoreWalls(unitType)-->void
--*gen.isCarryAir(unitType)-->boolean
--*gen.giveCarryAir(unitType)-->void
--*gen.removeCarryAir(unitType)-->void
--*gen.isParadrop(unitType)-->boolean
--*gen.giveParadrop(unitType)-->void
--*gen.removeParadrop(unitType)-->void
--*gen.isAlpine(unitType)-->boolean
--*gen.giveAlpine(unitType)-->void
--*gen.removeAlpine(unitType)-->void
--*gen.isBonusAgainstHorse(unitType)-->boolean
--*gen.giveBonusAgainstHorse(unitType)-->void
--*gen.removeBonusAgainstHorse(unitType)-->void
--*gen.isFreeSupportUnderFundamentalism(unitType)-->boolean
--*gen.giveFreeSupportUnderFundamentalism(unitType)-->void
--*gen.removeFreeSupportUnderFundamentalism(unitType)-->void
--*gen.isDestroyedAfterAttacking(unitType)-->boolean
--*gen.giveDestroyedAfterAttacking(unitType)-->void
--*gen.removeDestroyedAfterAttacking(unitType)-->void
--*gen.isBonusAgainstAir(unitType)-->boolean
--*gen.giveBonusAgainstAir(unitType)-->void
--*gen.removeBonusAgainstAir(unitType)-->void
--*gen.isSpotSubmarines(unitType)-->boolean
--*gen.giveSpotSubmarines(unitType)-->void
--*gen.removeSpotSubmarines(unitType)-->void
--#gen.wonderModifiedMoves(unit)-->integer
--#gen.maxMoves(unit) --> integer
--#gen.moveRemaining(unit) --> integer
--#gen.inPolygon(tile,tableOfCoordinates)-->bool
--#gen.cityCanSupportAnotherUnit(city)-->bool
--#gen.rehomeUnitsInCapturedCity(city,defender) --> void

--
-- FUNCTION IMPLEMENTATIONS
--
local gen = {}

-- gen.checkBits(integer,string)-->boolean
-- Compares the binary representation of an integer with
-- a string.  If the string has a 1 in a given place,
-- the binary representation of the integer should also
-- have a 1.  If the string has a 0 in a given place, the
-- binary representation should also have a 0. Any other
-- character in the string means the integer can have a
-- 0 or a 1.  If the integer representation is longer than
-- the string, the string is alligned with the smallest
-- part of the integer.
-- gen.checkBits(0b10101011,"xx10xwqp")-->true
-- gen.checkBits(0b10101011,"xx11xwqp")-->false
-- gen.checkBits(0b011110101011,"xx10xwqp")-->true
-- gen.checkBits(0b011110101011,"xx10xwqp")-->true
-- Helper function (provided to this library as checkBits and gen.checkBits)
-- note: lua does not actually accept integers specified in binary 
-- (though it does for hexidecimal)
--

local function checkBits(integer,bitString)
    local strlen = string.len(bitString)
    for i=1,strlen do
        local bitInt = 1<<(i-1)
        if bitString:sub(-i,-i) == "1" and integer & bitInt == 0 then
            -- ith bit isn't 1, but bitString specifies 1
            return false
        elseif bitString:sub(-i,-i) == "0" and integer & bitInt == bitInt then
            -- ith bit is 1, but bitString specifies 0
            return false
        end
    end
    -- if we get here, all specified bits match
    return true
end
gen.checkBits = checkBits

-- gen.setBits(integer,string)-->integer
-- sets binary bits in an integer to 1 or 0 based on
-- the information provided by a string.  Characters that 
-- are not 1 or 0 leave the corresponding bit unchanged
-- Last character of the string corresponds to the 1's bit
-- in the integer (string lines up to the least significant
-- part of the number)
-- gen.setBits(0b00000000,"xx10xxxx")-->0b00100000
-- gen.setBits(0b00000000,"xx10xx")-->0b00001000
-- gen.setBits(0b11111100,"xx0011xx")-->0b11001100
-- gen.setBits(0b10101011,"xx10xwqp")-->0b10101011
-- gen.setBits(0b10101011,"xx11xwqp")-->0b10111011
-- Helper function (provided to this library as setBits and gen.setBits)
-- note: lua does not actually accept integers specified in binary 
-- (though it does for hexidecimal)
local function setBits(integer,bitString)
    local strlen = string.len(bitString)
    for i=1,strlen do
        local bitInt = 1<<(i-1)
        if bitString:sub(-i,-i) == "1" then
            integer = integer | bitInt
        elseif bitString:sub(-i,-i) == "0" then
            integer = integer & ~bitInt
        end
    end
    return integer
end
gen.setBits = setBits

-- gen.printBits(integer,numOfBits or nil) --> string
-- prints the binary representation of integer,
-- including the numOfBits least significant bits
-- if numOfBits is nil, it defaults to 32
function gen.printBits(integer,numOfBits)
    if not numOfBits then
        numOfBits = 32
    end
    if type(integer)~= "number" or type(numOfBits) ~= "number" then
        error("gen.printBits requires integer arguments.")
    end
    local concatTable = {}
    for i=1,numOfBits do
        if integer & 1<<(numOfBits-i) == 0 then
            concatTable[i]="0"
        else
            concatTable[i]="1"
        end
    end
    return table.concat(concatTable)
end

-- gen.isBit1(integer,bitNumber)--> boolean
-- tells if bitNumber bit of integer is 1 
-- (1st bit is the bit for the ones position)
-- Helper Function (provided as local and in gen table)
local function isBit1(integer,bitNumber)
    return integer & 1<<(bitNumber-1) == 1<<(bitNumber-1)
end
gen.isBit1 = isBit1

-- gen.isBit0(integer,bitNumber)--> boolean
-- tells if bitNumber bit of integer is 0 
-- (1st bit is the bit for the ones position)
-- Helper Function (provided as local and in gen table)
local function isBit0(integer,bitNumber)
    return integer & 1<<(bitNumber-1) == 0
end
gen.isBit0 = isBit0

-- gen.setBit1(integer,bitNumber)-->integer
-- sets bitNumber bit of the integer to 1
-- (1st bit is the bit for the ones position)
-- Helper Function (provided as local and in gen table)
local function setBit1(integer,bitNumber)
	return integer | 1<<(bitNumber-1)
end
gen.setBit1 = setBit1

-- gen.setBit0(integer,bitNumber)-->integer
-- sets bitNumber bit of the integer to 0
-- (1st bit is the bit for the ones position)
-- Helper Function (provided as local and in gen table)
local function setBit0(integer,bitNumber)
	return integer & ~(1<<(bitNumber-1))
end
gen.setBit0 = setBit0

local thresholdTableMetatable = { __index = function(thresholdTable,key)
            if type(key) ~= "number" then
                return rawget(thresholdTable,key)
            else
                local bestIndexSoFar = -math.huge
                local bestValueSoFar = false
                for index,value in pairs(thresholdTable) do
                    if type(index) == "number" and key >= index and index >= bestIndexSoFar then
                        bestIndexSoFar = index
                        bestValueSoFar = value
                    end
                end
                return bestValueSoFar
            end
        end,}
-- A threshold table is a table where if a numerical key is indexed, and that
-- numerical key doesn't correspond to an index, the value of the largest
-- numerical index less than the key is used.
-- If there is no numerical index smaller than the key, false is returned
-- (nil is returned for non-numerical keys not in table)
-- Use an index -math.huge to provide values for arbitrarily small numerical keys
-- example 
-- myTable = gen.makeThresholdTable({[-1]=-1,[0]=0,[1]=1,})
-- myTable[-2] = false
-- myTable[-1] = -1
-- myTable[-0.6] = -1
-- myTable[3.5]=1
-- myTable["three"] = nil
-- myTable[0.5]=0
--
-- gen.makeThresholdTable(table or nil)-->thresholdTable
-- makes an input a threshold table or creates an empty thresholdTable
-- Also returns the table value
function gen.makeThresholdTable(inputTable)
    inputTable = inputTable or {}
    return setmetatable(inputTable,thresholdTableMetatable)
end

-- applyWonderBonus(wonderObject or integer,tribeObject or integer)-->boolean
-- gen.applyWonderBonus(wonderObject or integer,tribeObject or integer)-->boolean
-- returns true if the wonder has been built and is not
-- expired or destroyed 
-- integer means corresponding wonder/tribe id
local function applyWonderBonus(wonder,tribe)
    if type(wonder) == "number" then
        wonder = civ.getWonder(wonder)
    end
    if type(tribe) == "number" then
        tribe = civ.getTribe(tribe)
    end
    --check if expired
    for i=0,7 do
        if civ.getTribe(i) and wonder.expires and tribe:hasTech(wonder.expires) then
            return false
        end
    end
    if wonder.city and wonder.city.owner == tribe then
        return true
    else 
        return false
    end
end


-- toTile(tile or table)-->tile
-- gen.toTile(tile or table)-->tile
-- If given a tile object, returns the tile
-- If given coordinates for a tile, returns the tile
-- Causes error otherwise
-- Helper Function (provided to this library as toTile and gen.toTile)
--
local function toTile(input)
    if civ.isTile(input) then
        return input
    elseif type(input) == "table" then
        local xVal = input[1] or input["x"]
        local yVal = input[2] or input["y"]
        local zVal = input[3] or input["z"] or 0
        if type(xVal)=="number" and type(yVal)=="number" and type(zVal)=="number" then
            if civ.getTile(xVal,yVal,zVal) then
                return civ.getTile(xVal,yVal,zVal)
            else
                error("Table with values {"..tostring(xVal)..","..tostring(yVal)..
                        ","..tostring(zVal).."} does not correspond to a valid tile.")
            end
        else
            error("Table did not correspond to tile coordinates")
        end
    else
        error("Did not receive a tile object or table of coordinates.")
    end
end
gen.toTile = toTile


-- gen.hasIrrigation(tile)-->boolean
-- returns true if tile has irrigation but no farm
-- returns false otherwise
function gen.hasIrrigation(tile)
    tile = toTile(tile)
    local improvements = tile.improvements
    -- irrigation, but no mining, so not farmland
    if improvements & 0x04 == 0x04 and improvements & 0x08 == 0 then
        return true
    else
        return false
    end
end

-- gen.placeIrrigation(tile)-->void
-- places irrigation on the tile provided
-- removes mines and farmland if present
-- does nothing if tile has a city
function gen.placeIrrigation(tile)
    tile = toTile(tile)
    if tile.city then
        return
    end
    -- Set irrigation bit to 1
    tile.improvements = tile.improvements | 0x04
    -- Set mining bit to 0
    tile.improvements = tile.improvements & ~0x08
end

-- gen.removeIrrigation(tile)-->void
-- If tile has irrigation but no farmland, removes the irrigation
-- Does nothing to farmland
-- Does nothing if tile has a city
function gen.removeIrrigation(tile) 
    tile = toTile(tile)
    -- if tile has a city or farmland, do nothing
    if tile.city or tile.improvements & 0x0C == 0x0C then
        return
    end
    -- set irrigation bit to 0
    tile.improvements = tile.improvements & ~0x04
end

-- gen.hasMine(tile)-->boolean
-- function gen.hasMine(tile) end

-- gen.placeMine(tile)-->void
-- function gen.placeMine(tile) end

-- gen.removeMine(tile)-->void
-- function gen.removeMine end

-- gen.hasFarmland(tile)-->boolean
-- function gen.hasFarmland(tile) end

-- gen.placeFarmland(tile)-->void
-- function gen.placeFarmland(tile) end

-- gen.removeFarmland(tile)-->void
-- function gen.removeFarmland(tile) end

-- gen.hasRoad(tile)-->boolean
-- function gen.hasRoad(tile) end

-- gen.placeRoad(tile)-->void
-- function gen.placeRoad(tile) end

-- gen.removeRoad(tile)-->void
-- function gen.removeRoad(tile) end

-- gen.hasRailroad(tile)-->boolean
-- function gen.hasRailroad(tile) end

-- gen.placeRailroad(tile)-->void
-- function gen.placeRailroad(tile) end

-- gen.removeRailroad(tile)-->void
-- function gen.removeRailroad(tile) end

-- gen.hasFortress(tile)-->boolean
-- function gen.hasFortress(tile) end

-- gen.placeFortress(tile)-->void
-- function gen.placeFortress(tile) end

-- gen.removeFortress(tile)-->void
-- function gen.removeFortress(tile) end

-- gen.hasAirbase(tile)-->boolean
-- function gen.hasAirbase(tile) end

-- gen.placeAirbase(tile)-->void
-- function gen.placeAirbase(tile) end

-- gen.removeAirbase(tile)-->void
-- function gen.removeAirbase(tile) end

-- gen.hasPollution(tile)-->boolean
-- function gen.hasPollution(tile) end

-- gen.placePollution(tile)-->void
-- function gen.placePollution(tile) end

-- gen.removePollution(tile)-->void
-- function gen.removePollution(tile) end

-- gen.hasTransporter(tile)-->boolean
-- function gen.hasTransporter(tile) end

-- gen.placeTransporter(tile)-->void
-- function gen.placeTransporter(tile) end

-- gen.removeTransporter(tile)-->void
-- function gen.removeTransporter(tile) end
--
-- gen.isFortifying(unit)-->boolean
-- function gen.isFortifying(unit) end

-- gen.setToFortifying(unit)-->void
-- function gen.setToFortifying(unit) end

-- gen.isFortified(unit)-->boolean
-- function gen.isFortified(unit) end

-- gen.setToFortified(unit)-->void
-- function gen.setToFortified(unit) end

-- gen.isSleeping(unit)-->boolean
-- function gen.isSleeping(unit) end

-- gen.setToSleeping(unit)-->void
-- function gen.setToSleeping(unit) end

-- gen.isIrrigating(unit)-->boolean
-- function gen.isIrrigating(unit) end

-- gen.setToIrrigating(unit)-->void
-- function gen.setToIrrigating(unit) end

-- gen.isMining(unit)-->boolean
-- function gen.isMining(unit) end

-- gen.setToMining(unit)-->void
-- function gen.setToMining(unit) end

-- gen.isTransformingTerrain(unit)-->boolean
-- function gen.isTransformingTerrain(unit) end

-- gen.setToTransformingTerrain(unit)-->void
-- function gen.setToTransformingTerrain(unit) end

-- gen.isBuildingAirbase(unit)-->boolean
-- function gen.isBuildingAirbase(unit) end

-- gen.setToBuildingAirbase(unit)-->void
-- function gen.setToBuildingAirbase(unit) end

-- gen.isBuildingTransporter(unit)-->boolean
-- function gen.isBuildingTransporter(unit) end

-- gen.setToBuildingTransporter(unit)-->void
-- function gen.setToBuildingTransporter(unit) end

-- gen.isGoingTo(unit)-->boolean
-- function gen.isGoingTo(unit) end

-- gen.setToGoingTo(unit,tile)-->void
-- function gen.setToGoingTo(unit,tile) end

-- gen.isNoOrder(unit)-->boolean
-- function gen.isNoOrder(unit) end

-- gen.setToNoOrders(unit)-->void
-- function gen.setToNoOrders(unit) end

-- gen.isSeeTwoSpaces(unitType)-->boolean
-- function gen.isSeeTwoSpaces(unitType) end

-- gen.giveSeeTwoSpaces(unitType)-->void
-- function gen.giveSeeTwoSpaces(unitType) end

-- gen.removeSeeTowSpaces(unitType)-->void
-- function gen.removeSeeTowSpaces(unitType) end

-- gen.isIgnoreZOC(unitType)-->boolean
-- function gen.isIgnoreZOC(unitType) end

-- gen.giveIgnoreZOC(unitType)-->void
-- function gen.giveIgnoreZOC(unitType) end

-- gen.removeIgnoreZOC(unitType)-->void
-- function gen.removeIgnoreZOC(unitType) end

-- gen.isAmphibious(unitType)-->boolean
-- function gen.isAmphibious(unitType) end

-- gen.giveAmpibious(unitType)-->void
-- function gen.giveAmpibious(unitType) end

-- gen.removeAmphibious(unitType)-->void
-- function gen.removeAmphibious(unitType) end

-- gen.isSubmarine(unitType)-->boolean
-- function gen.isSubmarine(unitType) end

-- gen.giveSubmarine(unitType)-->void
-- function gen.giveSubmarine(unitType) end

-- gen.removeSubmarine(unitType)-->void
-- function gen.removeSubmarine(unitType) end

-- gen.isAttackAir(unitType)-->boolean
-- function gen.isAttackAir(unitType) end

-- gen.giveAttackAir(unitType)-->void
-- function gen.giveAttackAir(unitType) end

-- gen.removeAttackAir(unitType)-->void
-- function gen.removeAttackAir(unitType) end

-- gen.isCoastal(unitType)-->boolean
function gen.isCoastal(unitType)
    return isBit1(unitType.flags,6)
end

-- gen.giveCoastal(unitType)-->void
-- function gen.giveCoastal(unitType) end

-- gen.removeCoastal(unitType)-->void
-- function gen.removeCoastal(unitType) end

-- gen.isIgnoreWalls(unitType)-->boolean
-- function gen.isIgnoreWalls(unitType) end

-- gen.giveIngoreWalls(unitType)-->void
-- function gen.giveIngoreWalls(unitType) end

-- gen.removeIgnoreWalls(unitType)-->void
-- function gen.removeIgnoreWalls(unitType) end

-- gen.isCarryAir(unitType)-->boolean
-- function gen.isCarryAir(unitType) end

-- gen.giveCarryAir(unitType)-->void
-- function gen.giveCarryAir(unitType) end

-- gen.removeCarryAir(unitType)-->void
-- function gen.removeCarryAir(unitType) end

-- gen.isParadrop(unitType)-->boolean
-- function gen.isParadrop(unitType) end

-- gen.giveParadrop(unitType)-->void
-- function gen.giveParadrop(unitType) end

-- gen.removeParadrop(unitType)-->void
-- function gen.removeParadrop(unitType) end

-- gen.isAlpine(unitType)-->boolean
-- function gen.isAlpine(unitType) end

-- gen.giveAlpine(unitType)-->void
-- function gen.giveAlpine(unitType) end

-- gen.removeAlpine(unitType)-->void
-- function gen.removeAlpine(unitType) end

-- gen.isBonusAgainstHorse(unitType)-->boolean
-- function gen.isBonusAgainstHorse(unitType) end

-- gen.giveBonusAgainstHorse(unitType)-->void
-- function gen.giveBonusAgainstHorse(unitType) end

-- gen.removeBonusAgainstHorse(unitType)-->void
-- function gen.removeBonusAgainstHorse(unitType) end

-- gen.isFreeSupportUnderFundamentalism(unitType)-->boolean
-- function gen.isFreeSupportUnderFundamentalism(unitType) end

-- gen.giveFreeSupportUnderFundamentalism(unitType)-->void
-- function gen.giveFreeSupportUnderFundamentalism(unitType) end

-- gen.removeFreeSupportUnderFundamentalism(unitType)-->void
-- function gen.removeFreeSupportUnderFundamentalism(unitType) end

-- gen.isDestroyedAfterAttacking(unitType)-->boolean
-- function gen.isDestroyedAfterAttacking(unitType) end

-- gen.giveDestroyedAfterAttacking(unitType)-->void
-- function gen.giveDestroyedAfterAttacking(unitType) end

-- gen.removeDestroyedAfterAttacking(unitType)-->void
-- function gen.removeDestroyedAfterAttacking(unitType) end

-- gen.isBonusAgainstAir(unitType)-->boolean
-- function gen.isBonusAgainstAir(unitType) end

-- gen.giveBonusAgainstAir(unitType)-->void
-- function gen.giveBonusAgainstAir(unitType) end

-- gen.removeBonusAgainstAir(unitType)-->void
-- function gen.removeBonusAgainstAir(unitType) end

-- gen.isSpotSubmarines(unitType)-->boolean
-- function gen.isSpotSubmarines(unitType) end

-- gen.giveSpotSubmarines(unitType)-->void
-- function gen.giveSpotSubmarines(unitType) end

-- gen.removeSpotSubmarines(unitType)-->void
-- function gen.removeSpotSubmarines(unitType) end
--
--
-- gen.wonderModifiedMoves(unit)-->integer
-- returns the movement allowance of a unit after
-- taking into account nuclear power, wonders
-- returns atomic movement points
function gen.wonderModifiedMoves(unit)
    local fullHpMove = unit.type.move
    if unit.type.domain == 2 then
        -- apply nuclear power
        if unit.owner:hasTech(civ.getTech(59)) then
            fullHpMove = fullHpMove+totpp.movementMultipliers.aggregate
        end
        -- apply magellan's and lighthouse
        if applyWonderBonus(12,unit.owner) then
            fullHpMove = fullHpMove+2*totpp.movementMultipliers.aggregate
        end
        if applyWonderBonus(3,unit.owner) and not gen.isCoastal(unit.type) then
            fullHpMove = fullHpMove + totpp.movementMultipliers.aggregate
        end
    end
    return fullHpMove
end
--
-- maxMoves(unit)--> integer
-- gen.maxMoves(unit) --> integer
-- returns movement allowance for a unit after taking damage
-- into account, multiplied by the road/rail multiplier
-- Helper Function (provided as both local function and in table
function maxMoves(unit)
    local fullHpMove = unit.type.move
    if unit.type.domain == 2 then
        -- apply nuclear power
        if unit.owner:hasTech(civ.getTech(59)) then
            fullHpMove = fullHpMove+totpp.movementMultipliers.aggregate
        end
        -- apply magellan's and lighthouse
        if applyWonderBonus(12,unit.owner) then
            fullHpMove = fullHpMove+2*totpp.movementMultipliers.aggregate
        end
        if applyWonderBonus(3,unit.owner) and not gen.isCoastal(unit.type) then
            fullHpMove = fullHpMove + totpp.movementMultipliers.aggregate
        end
    end
    local moveAllowance = (unit.hitpoints*fullHpMove)//unit.type.hitpoints
    local moveMult = totpp.movementMultipliers.aggregate
    if moveAllowance % moveMult > 0 then
        moveAllowance = moveAllowance - moveAllowance % moveMult + moveMult
    end
    if unit.type.domain == 0 then
        return math.min(math.max( moveAllowance,moveMult),fullHpMove)
    elseif unit.type.domain == 1 then
        return fullHpMove
    elseif unit.type.domain == 2 then
        return math.min(math.max( moveAllowance,2*moveMult),fullHpMove)
    else
    end
end
gen.maxMoves = maxMoves

-- gen.moveRemaining(unit)
-- returns gen.maxMoves-unit.moveSpent
function gen.moveRemaining(unit)
    return gen.maxMoves(unit)-unit.moveSpent
end

--#gen.inPolygon(tile,tableOfCoordinates)-->bool
-- the table of coordinates defines the corners of the
-- polygon.  Returns true if the tile is within the
-- polygon defined by the table of coordinates, and
-- false otherwise.  Checking that the map is correct
-- must be done separately
-- Do not cross the date line with the polygon
--
--
-- Method (not necessary to understand for use)
-- (Note: I will use the "regular" mathematical coordinate system
-- in this explanation and code (i.e. positive y is "up" or "north").
-- All this does is reflect the coordinate system across the X-axis,
-- but it makes it easier for me to reason about.)
-- Suppose a line L connects the point (x,y) to
-- another point (X,Y), and (X,Y) is not in the
-- polygon.  If (x,y) is strictly in the polygon, the line
-- L will cross the boundary of the polygon an odd number
-- of times.  If (x,y) is strictly outside the polygon,
-- L will cross the boundary of the polygon an even number of
-- times.
-- It is easy enough to check the case where (x,y) is actually
-- on the boundary of the polygon, but difficulty arises
-- if a side of the polygon overlaps L.
-- Hence, we must avoid that case.
-- Also, there is some trouble in practice when L crosses a
-- vertex of the polygon (since it will look like it is
-- crossing two line segments, when there should only be a single
-- crossing counted.
--
-- For ease of calculation, it makes sense to use vertical
-- and/or horizontal lines for L, but this makes the likelihood
-- of a polygon side overlapping with L very high, and
-- runs in to the corner problem.
--
-- To avoid this, we will check points four points (x+-e,y+-e)
-- where e is small.  If only integer coordinates are used for the
-- polygon, this eliminates overlapping L with polygon sides, and
-- the possibility that L crosses a polygon vertex.
-- If any of the four points are considered "in" the
-- polygon, the tile is considered in the polygon, and
-- true is returned.  With e sufficiently small, this is unlikely
-- to "catch" tiles that "shouldn't" be in the polygon
--
function gen.inPolygon(tile,tableOfCoordinates)
    local function isNumericallyEqual(LHS,RHS)
        return math.abs(LHS-RHS) <= 1e-6
    end
    -- gets the Y value of the intersection of 
    -- L1 and L2, where
    -- L1 is defined as x=xStar
    -- L2 is the line through (x1,y1) and (x2,y2)
    local function getYStar(xStar,x1,y1,x2,y2)
        if x1==x2 then
            return nil
            -- the lines are parallel, either no solution
            -- or lines overlap
        end
        return ((y2-y1)/(x2-x1))*(xStar-x1)+y1
    end
    -- returns true if (a,b) is on the line
    -- segment defined by (x1,y1) and (x2,y2)
    -- note line segment, not entire line
    local function pointInSegment(a,b,x1,y1,x2,y2)
        local xLow,xHigh = math.min(x1,x2),math.max(x1,x2)
        local yLow,yHigh = math.min(y1,y2),math.max(y1,y2)
        local LHS = (b-y1)*(x2-x1)
        local RHS = (y2-y1)*(a-x1)
        if a<xLow or a>xHigh or b<yLow or b>yHigh then
            return false
        else
            return isNumericallyEqual(LHS,RHS)
        end
    end
    -- path(a,b) means the path from (a,b) to the point
    -- (a,-3) from here on
    -- returns true if path(a,b) crosses segment(x1,y1,x2,y2)
    local function pathCrossesSegment(a,b,x1,y1,x2,y2)
        -- case where the same point is entered twice
        if x1==x2 and y1==y2 then
            return isNumericallyEqual(a,x1) and isNumericallyEqual(b,y1)
        end
        local xLow,xHigh = math.min(x1,x2),math.max(x1,x2)
        -- I don't check for numerical equality here, since xi,yi should
        -- be integers, and a,b will be integers and a small perturbation 
        if a < xLow or a > xHigh then
            return false
        end
        -- here, the line (a,b) (a,-3) crosses the segment(x1,y1)(x2,y2),
        -- but we have to check if path(a,b) crosses the segment(x1,y1,x2,y2)
        -- first, get yStar
        local yStar = getYStar(a,x1,y1,x2,y2)
        -- since b>-3 (well, we can choose -math.huge instead of -3 for the end point
        -- it really doesn't matter), path(a,b) crosses segment(x1,y1,x2,y2) if 
        -- yStar <= b
        -- so check yStar<b and check for numerical equality also, just in case
        return (yStar < b) or isNumericallyEqual(b,yStar)
    end
    local e = 1e-3
    local point = {x=tile.x,y=tile.y}
    local northEast = {x=point.x+e,y=point.y+e}
    local northWest = {x=point.x-e,y=point.y+e}
    local southEast = {x=point.x+e,y=point.y-e}
    local southWest = {x=point.x-e,y=point.y-e}
    local northEastCrossings = 0
    local northWestCrossings = 0
    local southEastCrossings = 0
    local southWestCrossings = 0
    local numberOfVertices = #tableOfCoordinates
    if numberOfVertices == 0 then
        return false
    elseif numberOfVertices == 1 then
        return point.x == tableOfCoordinates[1][1] and point.y == tableOfCoordinates[1][2]
    end
    for i=1,numberOfVertices-1 do
        -- note, we'll deal with the segment between the last vertex and the first
        -- as a separate line
        local x1=tableOfCoordinates[i][1]
        local y1=tableOfCoordinates[i][2]
        local x2=tableOfCoordinates[i+1][1]
        local y2=tableOfCoordinates[i+1][2]
        if pointInSegment(point.x,point.y,x1,y1,x2,y2) then
            return true
        end
        if pathCrossesSegment(northEast.x,northEast.y,x1,y1,x2,y2) then
            northEastCrossings=northEastCrossings+1
        end
        if pathCrossesSegment(northWest.x,northWest.y,x1,y1,x2,y2) then
            northWestCrossings=northWestCrossings+1
        end
        if pathCrossesSegment(southEast.x,southEast.y,x1,y1,x2,y2) then
            southEastCrossings=southEastCrossings+1
        end
        if pathCrossesSegment(southWest.x,southWest.y,x1,y1,x2,y2) then
            southWestCrossings=southWestCrossings+1
        end
    end
    -- note, we'll deal with the segment between the last vertex and the first
    -- as a separate line
    local x1=tableOfCoordinates[numberOfVertices][1]
    local y1=tableOfCoordinates[numberOfVertices][2]
    local x2=tableOfCoordinates[1][1]
    local y2=tableOfCoordinates[1][2]
    if pointInSegment(point.x,point.y,x1,y1,x2,y2) then
        return true
    end
    if pathCrossesSegment(northEast.x,northEast.y,x1,y1,x2,y2) then
        northEastCrossings=northEastCrossings+1
    end
    if pathCrossesSegment(northWest.x,northWest.y,x1,y1,x2,y2) then
        northWestCrossings=northWestCrossings+1
    end
    if pathCrossesSegment(southEast.x,southEast.y,x1,y1,x2,y2) then
        southEastCrossings=southEastCrossings+1
    end
    if pathCrossesSegment(southWest.x,southWest.y,x1,y1,x2,y2) then
        southWestCrossings=southWestCrossings+1
    end
    -- if the number of crossings for any of these is odd, then we
    -- return true, since at least one of the four nearby coordinates
    -- is inside the polygon
    return (northEastCrossings % 2 == 1) or (northWestCrossings % 2 == 1)
        or (southEastCrossings % 2 == 1) or (southWestCrossings % 2 == 1)
end

-- gen.cityCanSupportAnotherUnit(city)-->bool
-- returns true if the city has enough production to support all existing
-- units and at least one other unit
-- Units that get free support under fundamentalism are still counted as
-- "supported", since they still take up a free support "slot" if they are
-- among the first 8 units supported by the city
function gen.cityCanSupportAnotherUnit(city)
    local unitsSupported = 0
    -- check that unit's location is a tile, otherwise dead units show
    -- up in the count
    for unit in civ.iterateUnits() do
        if unit.homeCity and unit.homeCity == city and unit.type.role <= 5 and
            civ.getTile(unit.location.x,unit.location.y,unit.location.z) then
            unitsSupported = unitsSupported +1
        end
    end
	local freeSupport = 0
	local govtNumber = city.owner.government
	if govtNumber <= 1 then
		-- anarchy or despotism
		freeSupport = city.size
	elseif govtNumber == 2 then
		-- monarchy
		freeSupport = civ.cosmic.supportMonarchy
	elseif govtNumber == 3 then
		-- communism
		freeSupport = civ.cosmic.supportCommunism
	elseif govtNumber == 4 then
		freeSupport = civ.cosmic.supportFundamentalism
	end
	return (freeSupport+city.totalShield - unitsSupported) > 0 
end

-- gen.rehomeUnitsInCapturedCity(city,defender) --> void
-- re-homes units in a captured city to other cities owned by
-- the same tribe, so that they are not disbanded
function gen.rehomeUnitsInCapturedCity(city,defender)
	local citySupportTable = {}
	for unit in civ.iterateUnits() do
    -- check that unit's location is a tile, otherwise dead units show
    -- up in the count
		if unit.homeCity and  civ.getTile(unit.location.x,unit.location.y,unit.location.z) and unit.type.role <= 5 then
			citySupportTable[unit.homeCity.id] = citySupportTable[unit.homeCity.id] or 0
			citySupportTable[unit.homeCity.id] = citySupportTable[unit.homeCity.id]+1
		end
	end
	local function canSupportAnotherUnit(city)
		local freeSupport = 0
		local govtNumber = city.owner.government
		if govtNumber <= 1 then
			-- anarchy or despotism
			freeSupport = city.size
		elseif govtNumber == 2 then
			-- monarchy
			freeSupport = civ.cosmic.supportMonarchy
		elseif govtNumber == 3 then
			-- communism
			freeSupport = civ.cosmic.supportCommunism
		elseif govtNumber == 4 then
			freeSupport = civ.cosmic.supportFundamentalism
		end
		-- make sure citySupportTable has an entry for this city
		citySupportTable[city.id] = citySupportTable[city.id] or 0
		return (freeSupport+city.totalShield - citySupportTable[city.id])> 0 	
    end
	local function taxiDistance(tileA,tileB)
		return math.abs(tileA.x-tileB.x)+math.abs(tileA.y-tileB.y)
	end
	for unit in civ.iterateUnits() do
		if unit.owner == defender and unit.homeCity == city and civ.getTile(unit.location.x,unit.location.y,unit.location.z) then
			local bestCitySoFar = nil
			local bestDistanceSoFar = 1000000
			for candidateCity in civ.iterateCities() do
				if candidateCity.owner == defender and canSupportAnotherUnit(candidateCity) 
					and taxiDistance(candidateCity.location,unit.location) <bestDistanceSoFar then
					bestCitySoFar = candidateCity
					bestDistanceSoFar = taxiDistance(bestCitySoFar.location,unit.location)
				end
			end
			unit.homeCity = bestCitySoFar
			if unit.type.role <= 5 then
				citySupportTable[bestCitySoFar.id]= (citySupportTable[bestCitySoFar.id] or 0)+1
			end
		end
	end
end











return gen
