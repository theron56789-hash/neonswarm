#include <3ds.h>
#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define W 400
#define H 240
static u8 *fb;
static void px(int x,int y,u32 c){if(x<0||y<0||x>=W||y>=H)return;u8*p=fb+3*(x*H+(H-1-y));p[0]=c&255;p[1]=(c>>8)&255;p[2]=c>>16;}
static void disc(int cx,int cy,int r,u32 c){for(int y=-r;y<=r;y++)for(int x=-r;x<=r;x++)if(x*x+y*y<=r*r)px(cx+x,cy+y,c);}
static void box(int x0,int y0,int w,int h,u32 c){for(int y=0;y<h;y++)for(int x=0;x<w;x++)px(x0+x,y0+y,c);}
static float rnd(void){return rand()/(float)RAND_MAX;}
typedef struct{float x,y,w,h;}R;
static R rs[8];static int nr;
static int hitR(float a,float b,float r,R o){float cx=fmaxf(o.x,fminf(a,o.x+o.w)),cy=fmaxf(o.y,fminf(b,o.y+o.h));return (a-cx)*(a-cx)+(b-cy)*(b-cy)<r*r;}
static int inR(float a,float b,float r){for(int i=0;i<nr;i++)if(hitR(a,b,r,rs[i]))return 1;return 0;}

static const char *DN[]={"Easy","Normal","Hard"},*MN[]={"Open","Pillars","Walls"};
static const int DHP[]={1,2,3};
static const float DDMG[]={5,8,12},DSP[]={.7f,.9f,1.1f},DSR[]={1.7f,1.1f,.8f},DFOOD[]={.4f,.25f,.15f},DBS[]={66,94,115};

typedef struct{float x,y,hp,f,hc;int s,on;}E;
typedef struct{float x,y,vx,vy;int on;}B;
typedef struct{float x,y,age;int k,on;}I;
static E en[60];static B bl[80],pb[40];static I it[16];
static float X,Y,php,pa,pinv,pf,tm,spt,nova;
static int pn,sc,best,di,mp,st; /* st: 0 menu, 1 play, 2 over */

static B *nb(B *a,int n){for(int i=0;i<n;i++)if(!a[i].on){memset(&a[i],0,sizeof(B));a[i].on=1;return &a[i];}return 0;}
static float er(E *e){return e->s?7:6;}

static void start(void){
  memset(en,0,sizeof en);memset(bl,0,sizeof bl);memset(pb,0,sizeof pb);memset(it,0,sizeof it);
  nr=0;
  if(mp==1){for(int a=0;a<3;a++)for(int b=0;b<2;b++)rs[nr++]=(R){100.f+a*100-12,72.f+b*96-12,24,24};}
  if(mp==2){rs[nr++]=(R){48,72,120,8};rs[nr++]=(R){232,168,120,8};rs[nr++]=(R){320,36,8,60};rs[nr++]=(R){40,144,8,60};}
  X=W/2;Y=H/2;php=100;pa=0;pinv=0;pf=0;tm=0;spt=1;nova=0;pn=2;sc=0;st=1;
}
static void drop(float x,float y){
  float r=rnd();int k=-1;
  if(r<DFOOD[di])k=0;else if(r<DFOOD[di]+.07f)k=rnd()<.6f?1:2;
  if(k<0)return;
  for(int i=0;i<16;i++)if(!it[i].on){it[i]=(I){x,y,0,k,1};return;}
}
static void update(void){
  const float dt=1/60.f;tm+=dt;
  circlePosition cp;hidCircleRead(&cp);u32 kh=hidKeysHeld();
  float vx=fabsf((float)cp.dx)>15?cp.dx/156.f:0,vy=fabsf((float)cp.dy)>15?-cp.dy/156.f:0;
  if(kh&KEY_DLEFT)vx-=1;
  if(kh&KEY_DRIGHT)vx+=1;
  if(kh&KEY_DUP)vy-=1;
  if(kh&KEY_DDOWN)vy+=1;
  float l=sqrtf(vx*vx+vy*vy);if(l>1){vx/=l;vy/=l;}
  float nx=X+vx*100*dt,ny=Y+vy*100*dt;
  if(!inR(nx,Y,6))X=fminf(W-6,fmaxf(6,nx));
  if(!inR(X,ny,6))Y=fminf(H-6,fmaxf(6,ny));
  pa+=4*dt;pinv-=dt;nova-=dt;
  spt-=dt;
  if(spt<=0){
    spt=fmaxf(.35f,1.4f-tm*.008f)*DSR[di];
    int s=rnd()<fminf(.35f,.1f+tm*.003f),q=rand()%4;
    for(int i=0;i<60;i++)if(!en[i].on){
      en[i]=(E){q<2?rnd()*W:(q==2?-10:W+10),q>=2?rnd()*H:(q==0?-10:H+10),(float)(DHP[di]+s),1+rnd()*2,0,s,1};break;}
  }
  for(int i=0;i<60;i++){E*e=&en[i];if(!e->on)continue;
    float dx=X-e->x,dy=Y-e->y,d=sqrtf(dx*dx+dy*dy);if(d<1)d=1;
    float sp=fminf(75,35+tm*.15f)*DSP[di];e->hc-=dt;
    if(e->s){
      if(d<95)sp=-sp*.6f;else if(d<130)sp=0;
      e->f-=dt;
      if(e->f<=0){e->f=2.2f*DSR[di];B*b=nb(bl,80);if(b){b->x=e->x;b->y=e->y;b->vx=dx/d*DBS[di];b->vy=dy/d*DBS[di];}}
    }
    e->x+=dx/d*sp*dt;e->y+=dy/d*sp*dt;
    if(d<er(e)+6&&pinv<=0){php-=DDMG[di];pinv=.6f;}
  }
  pf-=dt;
  if(pf<=0){
    int bi=-1;float bd=1e9f;
    for(int i=0;i<60;i++)if(en[i].on){float d=hypotf(en[i].x-X,en[i].y-Y);if(d<bd){bd=d;bi=i;}}
    if(bi>=0){B*s=nb(pb,40);if(s){s->x=X;s->y=Y;s->vx=(en[bi].x-X)/bd*240;s->vy=(en[bi].y-Y)/bd*240;}pf=.45f;}
  }
  for(int i=0;i<40;i++){B*s=&pb[i];if(!s->on)continue;
    s->x+=s->vx*dt;s->y+=s->vy*dt;
    if(s->x<0||s->y<0||s->x>W||s->y>H||inR(s->x,s->y,2)){s->on=0;continue;}
    for(int j=0;j<60;j++)if(en[j].on&&hypotf(en[j].x-s->x,en[j].y-s->y)<er(&en[j])+3){en[j].hp-=1;s->on=0;break;}
  }
  for(int k=0;k<pn;k++){
    float a=pa+k*6.2832f/pn,bx=X+cosf(a)*26,by=Y+sinf(a)*26;
    for(int j=0;j<60;j++)if(en[j].on&&en[j].hc<=0&&hypotf(en[j].x-bx,en[j].y-by)<er(&en[j])+5){en[j].hp-=1;en[j].hc=.3f;}
    for(int j=0;j<80;j++)if(bl[j].on&&hypotf(bl[j].x-bx,bl[j].y-by)<8)bl[j].on=0;
  }
  for(int i=0;i<80;i++){B*b=&bl[i];if(!b->on)continue;
    b->x+=b->vx*dt;b->y+=b->vy*dt;
    if(b->x<-5||b->y<-5||b->x>W+5||b->y>H+5||inR(b->x,b->y,3)){b->on=0;continue;}
    if(hypotf(b->x-X,b->y-Y)<9){if(pinv<=0)php-=DDMG[di]*.8f;b->on=0;}
  }
  for(int i=0;i<60;i++)if(en[i].on&&en[i].hp<=0){en[i].on=0;sc+=en[i].s?20:10;drop(en[i].x,en[i].y);}
  for(int i=0;i<16;i++){I*t=&it[i];if(!t->on)continue;
    t->age+=dt;if(t->age>10){t->on=0;continue;}
    if(hypotf(t->x-X,t->y-Y)<12){
      if(t->k==0)php=fminf(100,php+20);
      else if(t->k==1)pn=pn<6?pn+1:6;
      else{memset(bl,0,sizeof bl);nova=.5f;for(int j=0;j<60;j++)if(en[j].on&&hypotf(en[j].x-X,en[j].y-Y)<100)en[j].hp-=3;}
      t->on=0;
    }
  }
  if(php<=0){st=2;if(sc>best)best=sc;}
}
static void draw(void){
  for(int i=0;i<W*H;i++){u8*p=fb+3*i;p[0]=26;p[1]=7;p[2]=7;}
  for(int i=0;i<nr;i++)box((int)rs[i].x,(int)rs[i].y,(int)rs[i].w,(int)rs[i].h,0x8A5CFF);
  for(int i=0;i<16;i++)if(it[i].on)disc((int)it[i].x,(int)it[i].y,4,it[i].k==0?0x5DFF8A:it[i].k==1?0xFFE45C:0x19F0FF);
  for(int i=0;i<60;i++)if(en[i].on)disc((int)en[i].x,(int)en[i].y,(int)er(&en[i]),en[i].s?0xFF8A3B:0xFF3BD4);
  for(int i=0;i<80;i++)if(bl[i].on)disc((int)bl[i].x,(int)bl[i].y,3,0xFF5A5A);
  for(int i=0;i<40;i++)if(pb[i].on)disc((int)pb[i].x,(int)pb[i].y,2,0xFFFFFF);
  for(int k=0;k<pn;k++){float a=pa+k*6.2832f/pn;disc((int)(X+cosf(a)*26),(int)(Y+sinf(a)*26),5,0x19F0FF);}
  if(nova>0)for(int a=0;a<120;a++){float r=100*(1-nova*2);px((int)(X+cosf(a*.0524f)*r),(int)(Y+sinf(a*.0524f)*r),0x19F0FF);}
  disc((int)X,(int)Y,6,(pinv>0&&((int)(tm*20)&1))?0xFFFFFF:0x5AA8FF);
  box(6,6,100,6,0x222222);if(php>0)box(6,6,(int)php,6,php>35?0x5DFF8A:0xFF5A5A);
}
static void hud(int menu){
  consoleClear();
  if(menu==0){
    printf("\n  NEON SWARM\n\n  Up/Down: difficulty  < %s >\n  Left/Right: map  < %s >\n\n  Best: %d\n\n  A: play     START: quit\n\n  Circle Pad / D-Pad: move\n  You auto-fire. Blades smash\n  bullets. Green = heal,\n  yellow = blade, cyan = blast.\n",DN[di],MN[mp],best);
  }else if(menu==1){
    printf("\n  Score %d\n  Blades %d\n  %s / %s\n",sc,pn,DN[di],MN[mp]);
  }else{
    printf("\n  GAME OVER\n\n  Score %d\n  Best %d\n\n  A: menu\n",sc,best);
  }
}
int main(void){
  gfxInitDefault();consoleInit(GFX_BOTTOM,NULL);srand((unsigned)osGetTime());
  int frame=0,last=-1;
  while(aptMainLoop()){
    hidScanInput();u32 kd=hidKeysDown();
    if(kd&KEY_START)break;
    fb=gfxGetFramebuffer(GFX_TOP,GFX_LEFT,NULL,NULL);
    if(st==0){
      if(kd&KEY_UP)di=(di+2)%3;
      if(kd&KEY_DOWN)di=(di+1)%3;
      if(kd&KEY_LEFT)mp=(mp+2)%3;
      if(kd&KEY_RIGHT)mp=(mp+1)%3;
      if(kd&(KEY_UP|KEY_DOWN|KEY_LEFT|KEY_RIGHT))last=-1;
      if(kd&KEY_A){start();last=-1;}
    }else if(st==1){update();}
    else if(kd&KEY_A){st=0;last=-1;}
    draw();
    if(last!=st||(st==1&&frame%20==0)){hud(st);last=st;}
    frame++;
    gfxFlushBuffers();gfxSwapBuffers();gspWaitForVBlank();
  }
  gfxExit();return 0;
}
