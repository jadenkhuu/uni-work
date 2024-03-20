typedef struct node {
	int data;
	struct node *next;
} Node;

typedef Node *List;

int listCountOdds(list l) {
    if (l == NULL) {
        return 0;
    } 
    if (l->data % 2 != 0) {
        return listCountOdds(l->next) + 1;
    }
    else {
        return listCountOdds(l->next);
    }
}

List listDelete(list l, int value) {
    if (l == NULL) {
        return l;
    }
    if ()

}
