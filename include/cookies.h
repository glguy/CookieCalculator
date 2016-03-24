#ifndef COOKIES_H
#define COOKIES_H

typedef void (*row_callback)(void *token, const char *name, double cost, double delta);

struct cookie_summary {
        double cps, wrinkler_effect, munched,
               multiplier, banked, chain6_reserve,
               forfeit, earned, prestige, heavenly_chips;
        int wrinklers;
};

int analyze_cookie_save(const char *savefile, void *token, struct cookie_summary *summary, row_callback callback);

#endif
