#!/bin/lua

BIN  = arg[1]
LINE = '======================================================='..'==========='

-- sortie texte
local function printf(f, format, ...)
	f:write(string.format(format,...))
end
local function log(...) printf(io.stderr, ...) end
local function out(...) printf(io.stdout, ...) end

-- lit le binaire en mémoire
f = assert(io.open(BIN, 'rb')) mem = f:read('*all') f:close()

log('Read %s, %d bytes\n', BIN, mem:len())

-- lecture octets et mots
local function byte(addr) 
	local b = mem:byte(addr+1)
	if nil==b then error(addr) end
	return b
end
local function word(addr) 
	return byte(addr)*256+byte(addr+1) 
end

-- navigue entre lescfa/nfa/lfa/pfa
local function lfa2nfa(addr) return addr+2 end
local function nfa2cfa(addr) return addr+1+(byte(addr)%32) end
local function cfa2pfa(addr) return addr+2 end
local function pfa2cfa(addr) return addr-2 end
local function cfa2nfa(addr) 
	addr = addr-1 
	repeat addr = addr-1 until byte(addr)>=128 
	return addr 
end
local function nfa2lfa(addr) return addr-2 end

-- recup du nom
local function nfa2id(addr) 
	local id = ''
	if (byte(addr)%32)>0 then 
		repeat
			addr = addr + 1
			b = byte(addr)
			id = id .. string.char(b%128)
		until b>=128
	end
	return id
end

function lcomment(a, ...)
	local txt = ''
	for _,v in ipairs{...} do txt = txt..tostring(v) end
	out('lcomment %04X %s\n', a, txt:gsub('(.)', '\\%1')) 
end

-- gestion des labels
local lbl = {
	length = 7,
	l2s={},s2l={}, short = function(self, pfx, id)
		id = id and pfx..id or pfx
		local t,n = self.l2s[id]
		if t==nil then
			t,n = id:sub(1,self.length),0
			while self.s2l[t] do 
				t = id:sub(1,n>9 and self.length-2 or self.length-1)..n
				n = n+1
			end
			self.s2l[t], self.l2s[id] = id, t
			-- log('%s --> %s\n', id, t)
		end
		return t
	end,
	--
	pseudo_cfa = {},
	getLocal = function(self, addr, pseudo_cfa) 
		local w = dico.by_pfa[addr]
		if w then return self:short("p_",self:getSymb(w.id)) end
		local lbl = self[addr]
		if nil==lbl then
			lbl = string.format('Z%04X', addr)
			self[addr] = lbl
			out('label %04X %s\n', addr, lbl) 
			self.pseudo_cfa[addr] = pseudo_cfa
		end
		return lbl
	end,
	--
	charmap = {
		['.'] = 'dot',   [','] = 'kom',   ['/'] = 'div',   [':'] = 'col', 
		[';'] = 'semi',  ['+'] = 'add',   ['-'] = 'sub',   ['*'] = 'ast', 
		['='] = 'eq',    ['@'] = 'at',    ['!'] = 'exc',   ['|'] = 'bar', 
		['&'] = 'amp',   ['<'] = 'lt',    ['>'] = 'gt',    ['~'] = 'tild',
		['#'] = 'n_',    ['"'] = 'qt',    ["'"] = 'tck',   [' '] = 'spc',
		['('] = 'lp',    [')'] = 'rp',    ['['] = 'lsq',   [']'] = 'rsq', 
		['?'] = 'qm',    ['`'] = 'bq',    ['$'] = 'dol',   ['^'] = 'up'
	},
	symb = {['R>'] = 'from_r', ['>NEXT'] = 'next'}, bmys={},
	getSymb = function(self, id)
		local t = self.symb[id]
		if t==nil then
			t = id:lower()
				 :gsub('^<([^>]+)$',     'from_%1')
				 :gsub('^>([^<>]+)$',    'to_%1')
				 :gsub('^([^?]+)%?$',    'is_%1')
				 :gsub('^%(([^()]+)%)$', '_%1_')
				 :gsub('^%<([^<>]+)%>$', '_%1_')
				 :gsub('%%', 'per')
				 :gsub('.', function(c) return self.charmap[c] or c end) 
				 :gsub('[_]+','_')
				 :gsub('^_','')
			local s,n = t,0
			while self.bmys[t] do t,n = s..n,n+1 end
			self.symb[id],self.bmys[t] = t,id
		end
		return t
	end
}

local function new_word(lfa, dico)
	dico = dico or {by_lfa={},by_cfa={},by_id={},by_pfa={}}
	
	local nfa = lfa2nfa(lfa)
	local id  = nfa2id(nfa)
	local cfa = nfa2cfa(nfa)
	local pfa = cfa2pfa(cfa)
	local asm = (word(cfa)==pfa) or id=='>NEXT'
	
	local s,n = id,0
	while dico.by_id[id] do id,n=s..n,n+1 end
	
	local wrd = {id=id, lfa=lfa, nfa=nfa, cfa=cfa, asm=asm, pfa=pfa}
	dico.by_lfa[lfa] = wrd
	dico.by_cfa[cfa] = wrd
	dico.by_pfa[pfa] = wrd
	dico.by_id[id]   = wrd
	
	return dico
end

-- collect words from a cfa
local function words(lfa, ret)
	while lfa and lfa > 0 do
		ret = new_word(lfa, ret)
		lfa = word(lfa)
	end
	return ret
end

dico = nil
local lfa = {
	[0x02A3] = false, -- exception
	chk = function(self, addr)
		local r,b = self[addr]
		if nil==r then
			local function f(i) i = i + addr
				return 0<=i and i<mem:len() and byte(i) or 0
			end
			r,b = false,f(2) 
			if b>=128 and f(2+(b%32))>=128+33 then 
				r = true for i=3,1+(b%32) do b = f(i)
					if b<=32 or b>=128 then r = false break	end	
				end
				b = f(0)*256+f(1)
				r = r and (b==0 or self:chk(b))
			end
			-- print(string.format('%04X -> %d\n', addr, r and 1 or 0))
			self[addr] = r
		end
		return r
	end
}
for addr=8,mem:len()-4 do 
	if lfa:chk(addr) then 		
		dico = new_word(addr, dico) 
	end
end

-- ajoute les champs prev et next pour la navigaztion en mémoire
do
	local t = nil
	for a=0,mem:len()    do local w = dico.by_lfa[a] if w then w.prev, t = t, w end end
	t = nil
	for a=mem:len(),0,-1 do local w = dico.by_lfa[a] if w then w.next, t = t, w end end
end

-- prints vocabularies
local tmp, voc, count = {},{},0
for lfa,_ in pairs(dico.by_lfa) do tmp[lfa],count = true,count+1 end
for lfa,_ in pairs(dico.by_lfa) do tmp[word(lfa)] = nil end
for lfa,_ in pairs(tmp) do table.insert(voc,lfa) end
table.sort(voc, function(a,b) return a>b end)
out('insert 0 ; Found %d words over %d vocabularies\n', count, #voc)
for _,lfa in ipairs(voc) do 
	out('insert 0 ;\n');
	local t = string.format('$%04X:', lfa)
	while lfa>0 do
		local id = nfa2id(lfa2nfa(lfa))
		if t:len()+id:len()+1>=LINE:len() then 
			out('insert 0 ; %s\n', t:gsub('(.)','\\%1'))
			t='      ' 
		end
		t = t..' '..id
		lfa = word(lfa)
	end
	out('insert 0 ; %s\n', t:gsub('(.)','\\%1'))
end
out('insert 0\n')

function outAsm(w)
	lcomment(w.pfa-2, 'ASSEMBLER')

	-- on part du lfa suivant et on remonte jusqu'à JMP <NEXT ou BRA XX
	local next_lfa = (w.next and w.next.lfa) or mem:len()
	local end_code = next_lfa
	for a=next_lfa,w.pfa+2,-1 do
		if word(a-2)==0x0E12 or byte(a-2)==0x20 then 
			end_code = a
			break 
		end
	end
	-- out('code  %04X-%04X\n', w.pfa, end_code-1)
	-- on regrade si juste avant on a pas un swi
	-- out('const  %04X-%04X\n', w.pfa, end_code-1)
	out('code  %04X-%04X\n', w.pfa, end_code-1)
	if byte(end_code-4)==0x3F then -- swi
		-- out('code  %04X-%04X\n', w.pfa, end_code-4)
		out('data  %04X\n', end_code-3)
		out('byte  %04X\n', end_code-3)
		-- out('dec   %04X\n', end_code-3)
		out('code  %04X-%04X\n', end_code-2, end_code-1)
	end
	if end_code < next_lfa then
		out('data  %04X-%04X\n', end_code, next_lfa-1)
	end
end

function outId(a, w, ...)
	if w then lcomment(a, w.id, ...) end
end

function isJMP(w)
	return w.id=='?BRANCH' or w.id=='BRANCH' 
		or w.id=='(DO)'    or w.id=='(?DO)' 
		or w.id=='(LOOP)'  or w.id=='(+LOOP)' 
end

function isSTR(w)
	-- (word) iswhen the word is compiled not immediate
	local id = w.id
	if id=='FRP"' then return false end
	return id:sub(-1)=='"' 
		or id=='(."F)'
	    or (id:sub(1,1)=='(' and id:sub(-2)=='")') 
end

function plainData(deb, fin, const)
	if fin<=deb+6 then	
		out('byte %04X-%04X\n', deb, fin)
	else
		local fin2 = deb + 2*math.floor((fin + 1 - deb)/2) - 1
		if deb<fin2 then out('word %04X-%04X\n', deb, fin2) end
		if fin2<fin then out('byte %04X\n', fin) end
	end
	if const then out('const %04X-%04X\n',deb, fin)  end
end

function outStr(deb, fin, bit7)
	-- do out('byte %04X-%04X\n',deb,fin) return end
	out('data %04X\nbyte %04X\n',deb-1,deb-1)
	while deb<=fin do
		local zzz,t = math.min(deb+7,fin),''
		if zzz==deb+6 then zzz = deb+5 end
		-- if zzz==deb+4 then zzz = deb+2 end
		for i = deb,zzz do
			local  c = byte(i)
			if     c==7    then t = t..'\\a'
			elseif c==8    then t = t..'\\b'
			elseif c==9    then t = t..'\\t'
			elseif c==10   then t = t..'\\n'
			elseif c==13   then t = t..'\\r'
			elseif c<32    then t = string.format('%s\\%o', t, c)
			elseif c==34   then t = t..'\\"'
			elseif c<128   then t = t..string.char(c)
			elseif c==0x80 then t = t..'é'
			elseif c==0x81 then t = t..'ñ'
			elseif c==0x83 then t = t..'§'
			elseif c==0x84 then t = t..'è'
			elseif c==0x85 then t = t..'ù'
			elseif c==0x86 then t = t..'ç'
			elseif c==0x87 then t = t..'à'
			elseif c==0x88 then t = t..'°'
			elseif c==0x8a then t = t..'ä'
			elseif c==0x8b then t = t..'ë'
			elseif c==0x8c then t = t..'ï'
			elseif c==0x8d then t = t..'ö'
			elseif c==0x8e then t = t..'ü'
			elseif c==0x8f then t = t..'â'
			elseif c==0x90 then t = t..'ê'
			elseif c==0x91 then t = t..'î'
			elseif c==0x92 then t = t..'ô'
			elseif c==0x93 then t = t..'û'
			elseif bit7 and c>=128+32 then 
				t = t..string.char(c%128)
			else                
				t = string.format('%s\\%o', t, c)
			end
		end
		local qt = t:find('"') and "'" or '"'
		lcomment(deb, qt..t..qt)
		plainData(deb, zzz, true)
		deb = zzz+1
	end
end

function outDat(w)
	local immediate = w.immediate
	local function data(a, type)
		out('data %04X\n%s %04X\n', a, type or 'word', a) 
		return word(a)
	end

	local a, next_lfa = w.cfa, w.next and w.next.lfa or mem:len()
	local cfa_val = word(w.cfa)
	-- out('data  %04X-%04X\n', a, next_lfa-1)
	if cfa_val==0x1831 then 
		lcomment(w.cfa,'VARIABLE ',w.id) 
		lcomment(w.pfa, word(w.pfa)) 
		out('const %04X-%04X\n', w.pfa, next_lfa-1)
		return
	end
	if cfa_val==0x11B6 then 
		local val = word(w.pfa)
		lcomment(w.cfa,'CONSTANT ',w.id) 
		lcomment(w.pfa, val) 
		if -2<=val and val<=2 then out('const %04X\n', w.pfa) end
		-- out('const %04X\n', w.pfa)
		if w.pfa+2<=next_lfa-1 then
		out('const %04X-%04X\n', w.pfa+2, next_lfa-1)
		end
		return
	end
	if cfa_val==0x11F5 then lcomment(w.cfa,': ',w.id) end 
	while a+1<next_lfa do 
		local v = data(a)
		local w = dico.by_cfa[v]
		if w and w.id=='(LIT)' then
			v = data(a+2) out('const %04X\n', a+2)
			if v > 32 and dico.by_cfa[v] then -- ' word
				lcomment(a+2, "' ",dico.by_cfa[v].id)
			else -- un entier
				lcomment(a+2, ((v+32768)%65536)-32768)
			end
			a = a + 4
		elseif w and w.id=='(;CODE)' and not immediate then
			outId(a, w)
			out('code %04X-%04X\n', a+2,a+4) -- TODO lire jusqu'au jmp
			a = a + 5
		elseif w and isJMP(w) then
			v = data(a+2) 
			local lbl = dico.by_cfa[v] and "' "..dico.by_cfa[v].id 
			                            or lbl:getLocal(v,f)			
			outId(a, w, ' --',lbl,'--', v<a and '^' or 'v')
			a = a + 4
		elseif w and isSTR(w) and not immediate then
			local len = byte(a+2) 
			outId(a, w, ' len='..len)
			-- lcomment(a+2, "length = "..len)
			outStr(a+3,a+2+len, false)
			a = a+3+len
		else
			outId(a, w)
			a = a + 2
		end
	end
	if a+1 == next_lfa then
		out('data %04x\nbyte %04X\n', a,a)
	end
end

out('file %s\n',  BIN)
out('setdp 0\n')
out('option forced\n')
out('option flex\n')
out('option noasc\n')
out('option noconv\n')
out('option omitzero\n')
out('option nofcc\n')
out('option nohex\n')
out('label 0000 COLD\n')
out('label 0003 WARM\n')
out('label 0012 NEXT\n')
out('label 0006 MEMLIM\n')
out('word  0006\n')
out('byte  0008\n')

-- https://github.com/mamedev/mame/blob/master/src/mame/skeleton/squale.cpp
out('label f000 EF936X_REG_CMD\n')
out('label f001 EF936X_REG_CTRL1\n')
out('label f002 EF936X_REG_CTRL2\n')
out('label f003 EF936X_REG_CSIZE\n')
out('label f005 EF936X_REG_DELTAX\n')
out('label f007 EF936X_REG_DELTAY\n')
out('label f008 EF936X_REG_X_MSB\n')
out('label f009 EF936X_REG_X_LSB\n')
out('label f00a EF936X_REG_Y_MSB\n')
out('label f00b EF936X_REG_Y_LSB\n')
out('label f00c EF936X_REG_XLP\n')
out('label f00d EF936X_REG_YLP\n')

-- for _,a in ipairs{0x01c2, 0x01e1, 0x01fa, 0x45d, 0x1ae8} do 
	-- out('word %04X\n',a) 
	-- out('const %04X\n',a) 
-- end


for a = 0,mem:len()-1 do 
	local w = dico.by_lfa[a] if w then
		local sym = lbl:getSymb(w.id)
		
		out('insert %04X \n', w.lfa)
		out('comment %04X %s\n', w.lfa,  LINE)
		out('comment %04X %s\n', w.lfa, w.id:gsub('(.)','\\%1'))
		out('comment %04X %s\n', w.lfa,  LINE)
		out('data  %04X-%04X\n', w.lfa, w.pfa-1)
		
		out('label %04X %s\n', w.lfa, lbl:short('l_',sym))
		
		out('word  %04X\n', w.lfa)
		out('used  %04X\n', w.lfa)
		local lfa_w = dico.by_lfa[word(w.lfa)]
		if lfa_w then 
			lcomment(w.lfa, 'LFA -> ', lfa_w.id) 
		else
			out('const %04x\n', w.lfa)
		end
		
		out('insert %04X \n', w.nfa)
		out('label  %04X %s\n', w.nfa, lbl:short('n_',sym))
		out('bin    %04X\n', w.nfa)
		local t,s = byte(w.nfa)%128,''
		if t>=64 then t,s=t-64,s..' IMMEDIATE' w.immediate = true end
		if t>=32 then t,s=t-32,s..' SMUDGE'    w.smudge    = true end
		lcomment(w.nfa, "NFA -> "..t..s)
		outStr(w.nfa+1, w.cfa-1, true)	
		
		
		out('insert %04X \n', w.cfa)
		out('label  %04X %s\n', w.cfa, lbl:short('c_',sym))
		out('word   %04X\n', w.cfa)
		
		local cfa_w = word(w.cfa)
		if not dico.by_cfa[cfa_w] then -- pseudo-cfa
			out('label %04X %s\n', cfa_w, lbl:getLocal(cfa_w,true)) 
		end
		if w.next and w.pfa < w.next.lfa then -- i.e. hors constantes, variables
			out('label %04X %s\n', w.pfa, lbl:short('p_',sym))
			out('word %04X\n', w.pfa)
			-- out('const %04X\n', w.pfa)
		end
		if w.asm then 
			outAsm(w)
		else
			outDat(w)
		end
	end
end

-- add pseudo-cfa
for a in pairs(lbl.pseudo_cfa) do 
	local t,w=a-1
	repeat t,w = t-1,dico.by_lfa[t] until w or t==0
	if w then
		-- out('label %04X %s\n', a, lbl:short('a_', lbl:getSymb(w.id)))
		-- outAsm({pfa = a, next=w.next})
	end
end

-- std pseudo
out('label 11F5 do_col\n')
out('code 11F5-11FA\n')

out('label 11B6 do_const\n')
out('code 11B6-11BB\n')
out('label 11d2 do_2cst\n')
-- out('code 11d2-11d8\n')

out('label 1252 do_does\n')
out('code 1252-125c\n')

out('label 1831 do_var\n')
-- out('code 1831-1836\n')
out('label 16de do_voc\n')
-- out('word  16e1-16f6\n')

out('label 1abd getUVAR\n')
out('code 1abd-1ac5')
-- out('label 2ea6 UVAR\n')

out('label 0231 NEG_D\n')
out('label 029C NEG_D2\n')
out('label 0616 BYES\n')
out('label 3012 asm_idx\n')
out('label 32b3 asm_dp\n')
out('label 33ad asm_2\n')
out('label 33db asm_3\n')
out('label 3576 asm_23\n')
out('label 35F2 asm_lea\n')
out('label 3646 asm_cc\n')
out('label 369a asm_rl\n')
out('label 3745 asm_pp\n')
out('label 382e asm_cb\n')

out('CONST 6C5E\n') -- SWI

out('const 01c1\n')
out('const 01e1\n')
out('const 01ed\n')
out('const 01fa\n')
out('const 0207\n')
out('const 0309\n')
out('const 045d\n')
out('const 0667\n')
out('const 1ae8\n')

out('word 66d8\n')
out('label CC0E SYSDAT\n')

out('word 1ba6-1bb5\n')

out('code 201c-201e\n')