precision mediump float;

      uniform vec2 iResolution;
      uniform float iTime;
      uniform sampler2D iChannel0;

      varying highp vec2 vTextureCoord;

      void mainImage(out vec4 fragColor, in vec2 fragCoord);

      void main(void) {
        mainImage(gl_FragColor, vTextureCoord*iResolution);
      }
      // -----------------------------------------------------------------------
      // END - Common prelude
      // -----------------------------------------------------------------------

      // Created by mrange/2019
      // License Creative Commons Attribution-NonCommercial-ShareAlike 3.0 Unported License.

      // Based upon: https://www.shadertoy.com/view/3sXSD2

      // using a slightly adapted implementation of iq's simplex noise from
      // https://www.shadertoy.com/view/Msf3WH with hash(), noise() and fbm()

      vec2 hash (in vec2 p)
      {
        p = vec2 (dot (p, vec2 (127.1, 311.7)),
                  dot (p, vec2 (269.5, 183.3)));

        return -1. + 2.*fract (sin (p)*43758.5453123);
      }

      float noise (in vec2 p)
      {
        const float K1 = .366025404;
        const float K2 = .211324865;

        vec2 i = floor (p + (p.x + p.y)*K1);

        vec2 a = p - i + (i.x + i.y)*K2;
        vec2 o = step (a.yx, a.xy);
        vec2 b = a - o + K2;
        vec2 c = a - 1. + 2.*K2;

        vec3 h = max (.5 - vec3 (dot (a, a), dot (b, b), dot (c, c) ), .0);

        vec3 n = h*h*h*h*vec3 (dot (a, hash (i + .0)),
                               dot (b, hash (i + o)),
                               dot (c, hash (i + 1.)));

        return dot (n, vec3 (70.));
      }

      float fbm (in vec2 p, float time)
      {
        float c =  cos(time/sqrt(3.0));
        float d =  noise (p                 );
        d += .5*   noise (p + vec2(+c  ,+0.0));
        d += .25*  noise (p + vec2(+0.0,+c  ));
        d += .125* noise (p + vec2(-c  ,+0.0));
        d += .0625*noise (p + vec2(+0.0,-c  ));
        d /= (1. + .5 + .25 + .125 + .0625);
        return .5 + .5*d;
      }

      vec2 toPolar (in vec2 p)
      {
        float r = length (p);
        float a = atan (p.y, p.x);
        return vec2 (r, a);
      }

      vec2 toRect (in vec2 p)
      {
        float x = p.x*cos (p.y);
        float y = p.x*sin (p.y);
        return vec2 (x, y);
      }

      vec3 electricAlf(in vec2 uv)
      {
        const float thickness = 0.25;
        const float haze = 2.5;
        const float size = .075;
        const int count = 3;

        vec2 p = uv;

        vec2 pp = toPolar(p);
        pp.y += 0.2*p.x;
        p = toRect(pp);

        vec3 col = vec3(0.0);

        float a1 = smoothstep(0.05, 1.0, length(p - vec2(-0.4, 0.0)));
        float a2 = smoothstep(0.05, 1.0, length(p - vec2(0.4, 0.0)));
        float s1 = 1.0 / (a1 + 0.1)*1.1;
        float s2 = 1.0 / (a2 + 0.1)*1.1;

        float e1 = 1.6 + 0.4*sin(iTime*sqrt(2.0));
        float e2 = e1;

        for (int i = 0; i < count; ++i)
        {
          float fi = float(i);
          float time = iTime + fi;
          float fe1 = (pow(fi + 1.0, 0.2))*e1;
          float fe2 = fe1;
          vec2 o1 = 1.5*time*vec2(0,-1);
          vec2 o2 = o1;
          float d1 = abs ((p.y*haze)*thickness / (p.y - fe1*fbm (p + o1, time*0.11)*a1))*s1;
          float d2 = abs ((p.y*haze)*thickness / (p.y - fe2*fbm (p + o2, time*0.09)*a2))*s2;
          col += d1*size*vec3 (.1, .8, 2.);
          col += d2*size*vec3 (2., .1, .8);
        }

        col /= float(count-1);
        col += texture2D(iChannel0, -uv + vec2(0.5, 0.4)).xyz;
        return col;
      }

      void mainImage(out vec4 fragColor, in vec2 fragCoord)
      {
        vec2 uv = fragCoord.xy/iResolution.xy;
        uv = 2.0*uv - 1.0;
        uv.x *= iResolution.x/iResolution.y;

        vec3 col = electricAlf(uv*2.0);

        fragColor = vec4 (col, 1.);
      }