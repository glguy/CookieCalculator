typedef void (*row_callback)(void *token, const char *name, double cost, double delta);
int analyze_cookie_save(const char *savefile, void *token, row_callback callback);
