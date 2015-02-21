
Micah Jaffe's "insult" grammar
------------------------------------------------------


Posted-Date: Mon, 29 Apr 1996 19:11:35 -0700 (PDT)
Subject: insult grammar
To: zelenski@cs.Stanford.EDU
Date: Mon, 29 Apr 1996 19:11:59 -0700 (PDT)
From: Micah <zeade@cyclone.Stanford.EDU>
X-Mailer: ELM [version 2.4 PL25]

hi,

here's the file insult.g that i've come up with.  tee hee.
--
Insult grammar by Micah Jaffe (zeade@cyclone.stanford.edu)
It creates three basic types with even more sub-types, etc etc.
Hehe.  Some of the adjectives and <noun_prep>'s were taken from
the Macintosh hypercard stack "Random Insults."  The curses are all
"original" ;)

{
<start>
You <adj> <name> . ;
May <curse> . ;
}

Some others you may want to include, but I like the other two better:

You are so <adj1> that even a <noun> would not want to <good_verb> you . ;
With the <force> of <metaphor> , may <curse> . ;

adj1 deals more with um degrading things about the person abilities,
adj2 more about appearance (used when refering to the mutant beasties)

{
<adj>
<adj1> ;
<adj2> ;
<adj3> , <adj> ;
}

{
<adj3>
<adj1> ;
<adj2> ;
}

{
<adj1>
lame ;
dried up ;
par-broiled ;
bloated ;
half-baked ;
spiteful ;
egotistical ;
ungrateful ;
stupid ;
moronic ;
fat ;
ugly ;
puny ;
pitiful ;
insignificant ;
blithering ;
repulsive ;
worthless ;
blundering ;
retarded ;
useless ;
obnoxious ;
low-budget ;
assinine ;
neurotic ;
subhuman ;
crochety ;
indescribable ;
contemptible ;
unspeakable ;
sick ;
lazy ;
good-for-nothing ;
slutty ;
mentally-deficient ;
creepy ;
sloppy ;
dismal ;
pompous ;
pathetic ;
friendless ;
revolting ;
slovenly ;
cantankerous ;
uncultured ;
insufferable ;
gross ;
unkempt ;
defective ;
crumby ;
}

{
<adj2>
putrefied ;
festering ;
funky ;
moldy ;
leprous ;
curdled ;
fetid ;
slimy ;
crusty ;
sweaty ;
damp ;
deranged ;
smelly ;
stenchy ;
malignant ;
noxious ;
grimy ;
reeky ;
nasty ;
mutilated ;
sloppy ;
gruesome ;
grisly ;
sloshy ;
wormy ;
mealy ;
spoiled ;
contaminated ;
rancid ;
musty ;
fly-covered ;
moth-eaten ;
decaying ;
decomposed ;
freeze-dried ;
defective ;
petrified ;
rotting ;
scabrous ;
hirsute ;
}

{
<name>
<noun> ;
, bad excuse for <stuff> ;
, <stuff> for brains ;
, <adj2> <animal> <organics> for brains ;
offspring of a motherless <noun> ;
<noun_and_prep> <stuff> ;
<noun_and_prep> <adj2> <animal> <organics> ;
}

{
<stuff>
shit ;
toe jam ;
filth ;
puss ;
earwax ;
leaf clippings ;
bat guano ;
mucus ;
fungus ;
mung ;
refuse ;
earwax ;
spittoon spittle ;
phlegm ;
}

{
<noun_and_prep>
bit of ;
piece of ;
vat of ;
lump of ;
crock of ;
ball of ;
tub of ;
load of ;
bucket of ;
mound of ;
glob of ;
bag of ;
heap of ;
mountain of ;
load of ;
barrel of ;
sack of ;
blob of ;
pile of ;
truckload of ;
vat of ;
}

{
<organics>
droppings ;
mung ;
zits ;
puckies ;
tumors ;
cysts ;
tumors ;
livers ;
froth ;
parts ;
scabs ;
guts ;
entrails ;
blubber ;
carcuses ;
gizards ;
<body_parts> ;
}

{
<body_parts>
kidneys ;
genitals ;
buttocks ;
earlobes ;
innards ;
feet ;
}

{
<noun>
pop tart ;
warthog ;
twinkie ;
barnacle ;
fondue pot ;
cretin ;
fuckwad ;
moron ;
ass ;
neanderthal ;
nincompoop ;
simpleton ;
<animal> ;
}

{
<animal>
donkey ;
llama ; 
dingo ;
lizard ;
gekko ;
lemur ;
moose ;
camel ;
goat ;
eel ;
}

{
<good_verb>
love ;
cuddle ;
fondle ;
adore ;
smooch ;
hug ;
caress ;
worship ;
}

The curses...

{
<curse>
<afflictors> <bad_action> <place> ;
<afflictors> <adv> <bad_action> <place> ;
<afflictors> find your <body_parts> suddenly delectable ;
<afflictors> and <afflictors> seek a battleground <place> ;
}

{
<afflictors>
<quantity> <beasties> ;
<quantity> <beasties> ;
<quantity> <beasties> ;
<quantity> <beasties> ;
a <condition> Rush Limbaugh ;
the hosts of Hades ;
}

{
<quantity>
a <adj2> hoard of ;
a <adj2> pack of ;
a truckload of ;
a swarm of ;
many ;
an army of ;
a <adj2> heard of ;
a <adj2> platoon of ;
a <adj2> and <adj2> group of ;
<numbers> ;
}

{
<numbers>
a thousand ;
three million ;
ninty-nine ;
nine-hundred, ninty-nine ;
forty-two ;
a gazillion ;
sixty-eight times thirty-three ;
}

{
<adv>
viciously ;
manicly ;
merrily ;
happily ;
, with the <force> of <metaphor> , ;
gleefully ;
, with much ritualistic celebration , ;
franticly ;
}

{
<metaphor>
an irate manticore ;
Thor's belch ;
Alah's fist ;
<numbers> titans ;
a particularly vicious she-bear in the midst of her menstrual cycle ;
a pissed-off Jabberwock ;
}

{
<force>
force ;
fury ;
power ;
rage ;
}

{
<bad_action>
spit ;
shimmy ;
slobber ;
find refuge ;
find shelter ;
dance ;
retch ;
vomit ;
defecate ;
erect a strip mall ;
build a <bad_place> ;
have a religious experience ;
discharge bodily waste ;
fart ;
dance ;
drool ;
lambada ;
spill <numbers> rusty tacks ;
bite you ;
sneeze ;
sing <numbers> campfire songs ;
smite you <numbers> times ;
construct a new home ;
throw a party ;
procreate ;
}

{
<beasties>
yaks ;
<condition> maggots ;
<condition> cockroaches ;
stinging scorpions ;
fleas ;
<condition> weasels ;
<condition> gnats ;
South American killer bees ;
spiders ;
<adj2> monkeys ;
<condition> wiener-dogs ;
<condition> rats ;
<condition> wolverines ;
<adj2> , <condition> pit-fiends ;
}

{
<condition>
frothing ;
manic ;
crazed ;
plague-ridden ;
disease-carrying ;
biting ;
rabid ;
blood-thirsty ;
ravaging ;
slavering ;
}

{
<place>
in <relation> <in_something> ;
upon your mother's grave ;
on <relation> best rug ;
in the <bad_place> you call home ;
upon your heinie ;
}

{
<relation>
your ;
your ;
your ;
your father's ;
your mother's ;
your grandma's ;
}

{
<in_something>
entrails ;
anal cavity ;
shoes ;
house ;
pantry ;
general direction ;
pants ;
bed ;
}

{
<bad_place>
rat hole ;
sewer ;
toxic dump ;
oil refinery ;
landfill ;
porto-pottie ;
}

-- 
Micah Jaffe                                   zeade@cyclone.stanford.edu
DCC Consultant                                (415) 424-9532
"Crbcyr ner yvxr n obk bs pubpbyngrf.  Lhz." -- Sbeerfg Qnuyzre
