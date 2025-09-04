import { Page, expect } from "@playwright/test";

export class RepositoryPage {
  constructor(private page: Page) {}

  async verifyRepositoryPage(repoName: string) {
    await expect(this.page).toHaveURL(new RegExp(`/r/${repoName}`));
    await expect(
      this.page.getByRole("heading", { name: repoName }),
    ).toBeVisible();
  }

  async verifyBranchCount(expectedCount: string | RegExp) {
    await expect(this.page.locator("#branch-count")).toContainText(
      expectedCount,
    );
  }

  async verifyNoBranches() {
    await expect(this.page.locator("#branch-count")).toContainText(
      "0 branches",
    );
  }

  async verifyBranchesPresent() {
    await expect(this.page.locator("#branch-count")).not.toContainText(
      "0 branches",
      {
        timeout: 15000,
      },
    );
    await expect(this.page.locator("#branch-count")).toHaveText(
      /[1-9]\d* branches?/,
    );
  }

  async verifyBranchExists(branchName: string) {
    await expect(this.page.getByText(branchName)).toBeVisible();
  }

  async refreshRepository() {
    await this.page.getByRole("button", { name: /refresh/i }).click();
  }

  async deleteRepository() {
    this.page.on("dialog", (dialog) => dialog.accept());
    await this.page.getByRole("button", { name: /delete/i }).click();
    await this.page.waitForURL(/\/r$/);
  }
}
