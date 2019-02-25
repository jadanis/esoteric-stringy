#Stringy written by Josh Danis 2018-2019
import sys, os

dev_mode = False
subroutine = ''

def params(st,pt):
  pt = pt % len(st)
  s1 = st[:pt]
  c = st[pt]
  s2 = st[(pt+1):]
  return (c,s1,s2)

def com_f(res,npt):
  if dev_mode:
    print('Pointer at '+ str(npt) + ': ' + res)
  ps = params(res,npt)
  npt = npt % len(res)
  return hand(npt,*ps)

def help_f(f,s1):
  temp_m = map(ord,s1)
  temp_m = map(f,temp_m)
  temp_m = map(lambda x: x % 128,temp_m)
  temp_m = map(chr,temp_m)
  return ''.join(list(temp_m))

def idty(s1,s2,pt):
  res = s1+s2
  return com_f(res,pt)

def read(s1, s2, pt):
  inp = input()
  res = s1+inp+s2
  pt = len(s1 + inp)
  return com_f(res,pt)

def back_jump(s1,s2,pt):
  res = s1 + s2
  return com_f(res,pt-2)

def length(s1,s2,pt):
  res = s1 + chr(len(s1)) + s2
  return com_f(res,pt+1)

def skip(s1,s2,pt):
  res = s1 + s2
  return com_f(res,pt+1)

def mod(s1,s2,pt):
  modulo = ord(s2[0])
  res = help_f(lambda x: x % modulo,s1) + s2[1:]
  return com_f(res,pt)

def front(s1,s2,pt):
  res = s2[0] + s1 + s2[1:]
  return com_f(res,pt+1)

def back(s1,s2,pt):
  res = s1 + s2[1:] + s2[0]
  return com_f(res,pt)

def op1(s1,s2,pt):
  return

def plus(s1,s2,pt):
  n = ord(s2[0])
  res = help_f(lambda x: x + n, s1) + s2[1:]
  return com_f(res,pt)

def ret(s1,s2,pt):
  res = s1 + s2
  return com_f(res,0)

def minus(s1,s2,pt):
  n = ord(s2[0])
  res = help_f(lambda x: x - n, s1) + s2[1:]
  return com_f(res,pt)

def divi(s1,s2,pt):
  n = ord(s2[0])
  res = help_f(lambda x: x // n, s1) + s2[1:]
  return com_f(res,pt)

def appendDig(d):
  def r_func(s1, s2, pt):
    res = help_f(lambda x: 10*x + d,s1) + s2
    return com_f(res,pt)
  return r_func

def keep(s1,s2,pt):
  subroutine = s1
  return com_f(s2,0)

def subr(s1,s2,pt):
  res = hand(0,*params(s1,0)) + s2
  pt = len(res) - len(s2)
  return com_f(res,pt)

def shiftL(s1,s2,pt):
  res = help_f(lambda x: x << 1,s1) + s2
  return com_f(res,pt)

def eq(s1,s2,pt):
  res = s1[:-1] + str(int(s1[-1] == s2[0])) + s2[1:]
  return com_f(res,pt)

def shiftR(s1,s2,pt):
  res = help_f(lambda x: x >> 1,s1) + s2
  return com_f(res,pt)

def even(n):
  return n % 2 == 0

def shuff(s1,s2,pt):
  l = len(s1)
  split_At_Val = l // 2 if even(l) else (l // 2) + 1
  s1_star = s1[:split_At_Val]
  s1_star_2 = s1[split_At_Val:]
  s1 = ''.join([e for t in tuple(zip(s1_star,s1_star_2)) for e in t]) + (s1[split_At_Val-1] if not even(l) else '')
  res = s1 + s2
  return com_f(res,pt)

def jump(s1,s2,pt):
  res = s1 + s2[1:]
  pt = ord(s2[0])
  return com_f(res,pt)

def inc(s1,s2,pt):
  res = help_f(lambda x: x + 1, s2) + s2
  return com_f(res,pt)

def grteq(s1,s2,pt):
  res = s1[:-1] + str(int(s1[-1] > s2[0])) + s2[1:]
  return com_f(res,pt)

def dec(s1,s2,pt):
  res = help_f(lambda x: x - 1, s2) + s2
  return com_f(res,pt)

def piv(s1,s2,pt):
  res = s1[:-1] + s2[0] + s1[-1] + s2[1:]
  return com_f(res,pt+1)

def rep(s1,s2,pt):
  res = (s1 * (ord(s2[0]))) + s2[1:]
  pt = len(s1) * ord(s2[0])
  return com_f(res,pt)

def ifst(s1,s2,pt):
  if s2.find('}') >= 0:
    n = s2[::-1].find('}')
    res = (s1[:-1] + s2[:-n] + s2[(-n):] ) if not even(ord(s1[-1])) else (s1[:-1] + s2[(-n):])
    return com_f(res,(pt-1))
  else:
    res = s1[:-1] + s2 if not even(ord(s1[-1])) else s1[:-1]+s2[1:]
    return com_f(res,pt-1)

def call(s1,s2,pt):
  res = s1 + subroutine + s2
  pt = len(s1 + subroutine)
  return com_f(res,pt)

def rev(s1,s2,pt):
  res = s1[::-1] + s2
  return com_f(res,pt)

def delete(s1,s2,pt):
  return com_f(s2,0)

def end(s1,s2,pt):
  return s1+s2

coms = {
    ' ' : read,
    '!' : back_jump,
    '"' : idty,
    '#' : length,
    '$' : skip,
    '%' : mod,
    '&' : idty,
    '\'' : idty,
    '(' : front,
    ')' : back,
    '*' : op1,
    '+' : plus,
    ',' : ret,
    '-' : minus,
    '.' : end,
    '/' : divi,
    '0' : appendDig(0),
    '1' : appendDig(1),
    '2' : appendDig(2),
    '3' : appendDig(3),
    '4' : appendDig(4),
    '5' : appendDig(5),
    '6' : appendDig(6),
    '7' : appendDig(7),
    '8' : appendDig(8),
    '9' : appendDig(9),
    ':' : keep,
    ';' : subr,
    '<' : shiftL,
    '=' : eq,
    '>' : shiftR,
    '?' : shuff,
    '@' : jump,
    '[' : inc,
    '\\' : grteq,
    ']' : dec,
    '^' : piv,
    '_' : rep,
    '`' : idty,
    '{' : ifst,
    '|' : call,
    '}' : idty,
    '~' : rev,
    '\DEL' : delete
}


def hand(pt,c,s1,s2):
  try:
    command = coms[c]
    return command(s1,s2,pt)
  except KeyError:
    return hand(pt+1,*params(s1+c+s2,pt+1))


def run_stringy(st):
  print(hand(0,*params(st,0)))

def run_stringy_dev(st):
  global dev_mode
  dev_mode=True
  run_stringy(st)

def run_stringy_loop(file_path):
  file_name, file_ext = os.path.splittext(file_path)
  if file_ext != '.sty':
    print("Whoops! This does not seem to be a Stringy file!")
  else:
    try:
      f = open(file_path)
      bigS = f.read()
      if all( (31 < ord(c) < 127) or (ord(c) == 10) or (ord(c) == 9) for c in bigS):
        progs = f.readlines()
        f.close()
        for p in progs:
          if p[0] != '\t':
            run_stringy(p)
      else:
        print("This appears not to be a valid Stringy file!")
   except:
    print("Whoops! Something went wrong opening this file!")

def run_stringy_loop_dev(file_path):
  global dev_mode
  dev_mode=True
  run_stringy_loop(file_path)
