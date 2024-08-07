# The worst possible NYT Spelling Bee Words

I wrote this simple program to find the worst possible spelling bee pangrams, in the sense that they give the fewest possible answers.

I explained this code in two blog posts:

- [part 1](https://notes.billmill.org/blog/2024/03/What_are_the__worst__spelling_bee_pangrams_.html)
- [part 2](https://notes.billmill.org/blog/2024/03/mitzVah_-_the__worst__pangrams_part_2.html)

I didn't have the actual spelling bee glossary, so I used a larger word list, and these words actually have fewer matches than presented here.

```
$ python worst.py
words	pangram
9       equivoke       equivoque,evoke,keek,keeve,kook,kookie,queue,vive
14      cazique        acacia,acequia,aecia,aqua,aquae,caca,cacique,caeca,caique,c...
21      quirkily       illy,kill,kirk,klik,krill,kukri,kuru,lily,liri,lull,lulu,lu...
22      exiguity       eggy,etui,exegete,exit,gigue,gite,guitguit,gutty,tegg,text,...
23      jukebox        beebee,boob,booboo,book,bookoo,boubou,bubo,bubu,ebook,jeux,...
23      pirozhki       hippo,hook,hoop,hoopoo,horror,iroko,kirk,kook,koph,orzo,phi...
23      pirozhok       hippo,hook,hoop,hoopoo,horror,iroko,kirk,kook,koph,orzo,phi...
23      quixotic       cocci,coccic,coco,coocoo,coot,coquito,cutout,ictic,otic,oti...
24      kiwifruit      frit,fritt,fruit,furfur,kirk,kiwi,kufi,kukri,kuru,riff,rift...
24      wickyup        cuppy,icky,kick,kickup,kicky,kiwi,pick,pickup,pickwick,pick...
25      quixote        etiquette,etui,exit,otto,outquote,queue,quiet,quit,quite,qu...
26      euphemize      emeu,epee,heeze,heme,hemp,hempie,hippie,hump,humph,imphee,i...
26      foxhound       dodo,doff,doodoo,doux,duff,fohn,fond,fondu,food,found,fund,...
26      gimmickry      cirri,crick,gimmick,gimmicky,grig,grigri,grim,grimy,gyri,ic...
27      jacquard       ajar,aqua,audad,aura,aurar,caca,caracara,card,carr,caudad,c...
```

_update_: Brad Greenlee kindly [provided a better wordlist](https://fiasco.social/@brad/112108107787262025) he used in [his spelling bee solver](https://github.com/bgreenlee/spelling-beat/blob/main/js/application.js#L4). Here are the results with that list; this time I sorted by the total score of the word set rather than the number of words:

```
$ python worst.py words2.txt
words	points	pangram
17      33      fuckwit        cuff,fuck,kick,kiwi,kufi,tick,tiff,tiki,tuck,tuft,tutti,tut...
13      34      jacquard       ajar,aqua,aura,card,crud,curd,juju,qajaq,quad,radar,raja
6       35      equivoke       equivoque,evoke,kook,queue
12      37      exiguity       eggy,exit,gigue,itty,text,tutee,tutti,tutu,yegg,yeti
15      37      jukebox        boob,booboo,book,jeux,joke,jouk,jube,juju,jujube,juke,juku,...
19      40      puffbird       biff,bird,buff,burb,burd,burfi,burp,burr,drib,drip,drub,dru...
14      42      zygotic        city,cocci,coot,cozy,itty,oozy,tizzy,toot,yogi,yogic,ziti,z...
21      42      hindmilk       dill,dink,hill,hind,kilim,kill,kiln,kind,kink,limn,link,mid...
13      44      vibraharp      aria,baba,barb,brava,briar,hair,harp,pair,papa,pariah,rabbi
15      46      whirlwind      dill,dirndl,drill,hill,hind,rill,rind,whir,whirl,whirr,wild...
20      46      kiwifruit      fruit,kiwi,kufi,kukri,kuru,riff,rift,ruff,tiff,tiki,tuft,tu...
18      47      wamefou        ammo,fame,femme,foam,fume,mama,mamma,meme,memo,meow,momma,m...
10      48      cazique        acacia,acai,acequia,aqua,cacique,caique,queue,quiz
18      50      viewbook       bike,boob,booboo,book,bookie,bowwow,evoke,kiwi,kook,oboe,vi...
23      51      workgroup      goop,gorp,grog,grok,group,grow,guru,kook,kuru,pogo,poop,poo...
```

It's a hilarious result that `fuckwit` comes out as the lowest-scoring possible pangram.

One thing I realized eventually is that I'm not taking into account the required letter constraint; for example if we choose `jacquard` as our pangram and require the letter `q`, that limits us to just the words `aqua`, `qajaq`, and `quad`, which means that `jacquard`'s total score would be 22, 15 of which would be the pangram itself.

Here's a list of the 22 pangrams, including a required letter, that score the lowest in the word list given by `words3.txt`:

| points | pangram      | words          |
| ------ | ------------ | -------------- |
| 14     | kamoti**q**  |
| 14     | mitz**v**ah  |
| 14     | princo**x**  |
| 15     | **v**agotomy |
| 15     | **v**iburnum |
| 15     | conflu**x**  | flux           |
| 15     | jukebo**x**  | jeux           |
| 15     | ca**z**ique  | quiz           |
| 15     | ga**z**pacho |
| 16     | bo**v**inity | viny           |
| 16     | checkbo**x** | exec           |
| 16     | fo**x**hound | doxx           |
| 16     | qui**x**ote  | exit,text      |
| 17     | **h**indmilk | hill,hind      |
| 17     | b**i**keway  | bike,kiwi,wiki |
| 17     | **j**udicial | jail,juju      |
| 17     | **w**amefou  | meow,wame,woof |
| 17     | e**x**iguity | exit,text      |
| 17     | tubife**x**  | exit,ibex,text |
| 17     | activi**z**e | zeta,ziti      |
| 18     | ja**c**quard | card,crud,curd |
| 18     | pu**ff**back | buff,cuff,puff |
| 18     | hi**g**hjack | gaga,high,jagg |
