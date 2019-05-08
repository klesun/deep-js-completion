
let _sligify_cache_regexp: string[] = [];

export function _slugify_replace(input: string, options: IOptions, unsafe2: boolean): string
{
    if (unsafe2) {
        _sligify_cache_regexp
            .reduce(function (input, r)
            {
                return input.replace(r[0], r[1]);
            }, input);
    }
}